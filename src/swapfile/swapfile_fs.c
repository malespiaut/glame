/*
 * swapfile_fs.c
 *
 * Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004 Richard Guenther
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */
/* Swapfile implementation using a native filesystem. The idea is to
 * implement cluster sharing by having seperate native files for clusters.
 * A swapfile file consists of a file containing the used cluster numbers.
 * A cluster consists of two files, one for the data and one for listing
 * all files that use this cluster.
 * Main directory layout is
 * [swapfile directory]/
 *   clusters.data/
 *     {long}
 *   clusters.meta/
 *     {long}
 *   {long}
 * The {long} indexed clusters and files are hashed and the metadata
 * is loaded on demand.
 */

#define _XOPEN_SOURCE 500

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef SWDEBUG
#define DEBUG 1
#endif

#include <sys/stat.h>
#include <sys/types.h>
#include <pthread.h>
#include <fcntl.h>
#include <dirent.h>
#include <unistd.h>
#include <signal.h>
#include <stdio.h>
#include <setjmp.h>
#include <string.h>
#include <errno.h>
#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif
#include "list.h"
#include "hash.h"
#include "swapfile.h"


/* Some operations have two implementations, one cooked up out
 * of swapfile API functions, one out of lowlevel ones. Other
 * have an optimized hardcoded path for common usage patterns. */
#undef USE_COOKED_OPS_WRITE
#undef USE_COOKED_OPS_READ


/* The global "state" of the swapfile and its locks. The
 * locks are for namespace operations atomicy. */
static struct {
	char *files_base;
	char *clusters_data_base;
	char *clusters_meta_base;
	struct glame_list_head fds;
	int fsck, ro, clean, panic;
} swap = { NULL, NULL, NULL, };
#define SWAPFILE_OK() (swap.files_base != NULL)
#define SWAPFILE_RW() (SWAPFILE_OK() && (!swap.ro || swap.fsck))
#define SWAPFILE_MARK_UNCLEAN(msg) do { DPRINTF("UNCLEAN: " msg "\n"); swap.clean = 0; swap.ro = 1; } while (0)

static pthread_mutex_t swmx = PTHREAD_MUTEX_INITIALIZER;
#define LOCK do { pthread_mutex_lock(&swmx); } while (0)
#define UNLOCK do { pthread_mutex_unlock(&swmx); } while (0)

static void (* panic_handler)(const char *msg) = NULL;
#ifdef HAVE_HCC
static void do_panic(const char *msg) __attribute__((noreturn));
#else
static void do_panic(const char *msg) __attribute__((noreturn));
#endif
#define SWPANIC(msg) do { if (swap.panic) PANIC(msg); else do_panic(msg); } while (0)


/* We want to have a clean (static) namespace... */
#include "swfs_ctree.c"
#include "swfs_cluster.c"
#include "swfs_file.c"


/* The FILE equivalent - the swapfile filedescriptor. No locking here
 * as it is done by fd->file locking. Also the user has to ensure
 * consistency himself. */
struct swfd;
struct swfd {
	struct swfd *next_swfd_hash;
	struct swfd **pprev_swfd_hash;
	struct glame_list_head list;
	struct swfile *file;
	swfd_t fd;
	int mode;
	s64 offset;          /* file pointer position */
};
#define glame_list_add_swfd(fd) glame_list_add(&(fd)->list, &swap.fds)
#define glame_list_del_swfd(fd) glame_list_del_init(&(fd)->list)
HASH(swfd, struct swfd, 8,
     (swfd->fd == fd),
     (fd),
     (swfd->fd),
     swfd_t fd)


/* Forwards of helpers.
 */
static int fsck_scan_clusters(int fix);
static int fsck_file(struct swfile *file, int fix);
static int fsck_check_files(int fix);
static int fsck_cluster(struct swcluster *cluster, int fix);
static int fsck_check_clusters(int fix);


/* Fatal error handling.
 */

static void do_panic(const char *msg)
{
	swap.panic = 1;
	if (panic_handler)
		panic_handler(msg);
	file_sync();
	PANIC(msg);
}


/**********************************************************************
 * Initialization/cleanup
 */

static int _swapfile_init(const char *name, int force)
{
	char str[256];
	struct stat st;
	long hash;
	int fd;
	FILE *f;

	/* Check for correct swapfile directory setup:
	 * - main directories, clusters.data & clusters.meta
	 * - hash directories. */
	if (stat(name, &st) == -1
	    || !S_ISDIR(st.st_mode) || !((st.st_mode & S_IRWXU) == S_IRWXU)) {
		errno = ENOENT;
		return -1;
	}
	snprintf(str, 255, "%s/clusters.data", name);
	if (stat(str, &st) == -1
	    || !S_ISDIR(st.st_mode) || !((st.st_mode & S_IRWXU) == S_IRWXU)) {
		errno = ENOENT;
		return -1;
	}
	snprintf(str, 255, "%s/clusters.meta", name);
	if (stat(str, &st) == -1
	    || !S_ISDIR(st.st_mode) || !((st.st_mode & S_IRWXU) == S_IRWXU)) {
		errno = ENOENT;
		return -1;
	}
	for (hash = 0; hash < 256; hash++) {
		snprintf(str, 255, "%s/clusters.data/%lX", name, hash);
		if (stat(str, &st) == -1
		    || !S_ISDIR(st.st_mode)
		    || !((st.st_mode & S_IRWXU) == S_IRWXU)) {
			if (hash == 0)
				fprintf(stderr,
"HASH missing - maybe you're running a 0.4 swap?\n"
"Try using glame-convert.sh to convert it to 0.5 format\n");
			errno = ENOENT;
			return -1;
		}
	}
	for (hash = 0; hash < 256; hash++) {
		snprintf(str, 255, "%s/clusters.meta/%lX", name, hash);
		if (stat(str, &st) == -1
		    || !S_ISDIR(st.st_mode)
		    || !((st.st_mode & S_IRWXU) == S_IRWXU)) {
			errno = ENOENT;
			return -1;
		}
	}

	/* Try to place a .lock file exclusievely. */
	snprintf(str, 255, "%s/.lock", name);
	if ((fd = open(str, O_RDWR|O_CREAT|O_EXCL, 0666)) == -1 && !force) {
		errno = EBUSY;
		return -1;
	}
	if (fd == -1) {
		long pid;
		if (!(f = fopen(str, "r+")))
			return -1;
		if (fscanf(f, "%li", &pid) == 1) {
			if (kill(pid, 0) == 0 || errno != ESRCH) {
				fclose(f);
				errno = EBUSY;
				return -1;
			}
		}
		fclose(f);
		unlink(str);
		if ((fd = open(str, O_RDWR|O_CREAT|O_EXCL, 0666)) == -1) {
			errno = EBUSY;
			return -1;
		}
	}
	f = fdopen(fd, "w");
	fprintf(f, "%li\nGLAME " VERSION "\n", (long)getpid());
	fclose(f);

	/* Initialize swap structure. */
	swap.files_base = strdup(name);
	snprintf(str, 255, "%s/clusters.data", name);
	swap.clusters_data_base = strdup(str);
	snprintf(str, 255, "%s/clusters.meta", name);
	swap.clusters_meta_base = strdup(str);
	GLAME_INIT_LIST_HEAD(&swap.fds);
	swap.fsck = 0;
	swap.ro = 0;
	swap.clean = 1;
	swap.panic = 0;

	/* Initialize cluster subsystem. */
	if (cluster_init(2048, 128, 256, 256*1024*1024) == -1)
		return -1;

	return 0;
}

void swapfile_register_panic_handler(void (*handler)(const char *))
{
	panic_handler = handler;
}

/* Tries to open an existing swap file/partition.
 * Returns 0 on success, -1 on failure.
 * Failures can be
 *  - missing swap file/parition
 *  - in use swap
 *  - unclean swap */
int swapfile_open(const char *name, int flags)
{
	if (SWAPFILE_OK()) {
		errno = EINVAL;
		return -1;
	}

	if (_swapfile_init(name, 0) == -1)
		return -1;

	/* Do basic fsck to check if we're really clean. */
	if (/* fsck_scan_clusters(0) == 1
	       || */ fsck_check_files(0) == 1
	    /* || fsck_check_clusters(0) == 1 */) {
		/* Doh - unclean - close swap, but leave .lock */
		swap.clean = 0;
		swapfile_close();
		errno = EBUSY;
		return -1;
	}

	return 0;
}

void swapfile_sync()
{
	file_sync();
}

/* Closes and updates a previously opened swap file/partition
 * and marks it clean. */
void swapfile_close()
{
	struct swfd *f;
	char s[256];

	if (!SWAPFILE_OK())
		return;

	/* Close all files */
	while ((f = glame_list_gethead(&swap.fds, struct swfd, list))) {
		sw_close(f->fd);
	}

	/* Cleanup cluster subsystem. */
	cluster_cleanup();

	/* Remove the .lock file - only, if clean, else force fsck
	 * on next start. */
	if (swap.clean) {
		snprintf(s, 255, "%s/.lock", swap.files_base);
		unlink(s);
	} else
		DPRINTF("WARNING! Unclean swap. Will force FSCK.\n");

	/* Cleanup swap structure. */
	free(swap.files_base);
	free(swap.clusters_data_base);
	free(swap.clusters_meta_base);
	swap.files_base = NULL;
	swap.clusters_data_base = NULL;
	swap.clusters_meta_base = NULL;
}

/* Tries to create an empty swapfile on name of size size. */
int swapfile_creat(const char *name, size_t size)
{
	char s[256];
	long i;

	if (mkdir(name, 0777) == -1)
		return -1;
	snprintf(s, 255, "%s/clusters.meta", name);
	if (mkdir(s, 0777) == -1)
		return -1;
	snprintf(s, 255, "%s/clusters.data", name);
	if (mkdir(s, 0777) == -1)
		return -1;

	/* Set up the "hashes" */
	for (i=0; i<256; i++) {
		snprintf(s, 255, "%s/clusters.meta/%lX", name, i);
		if (mkdir(s, 0777) == -1)
			return -1;
		snprintf(s, 255, "%s/clusters.data/%lX", name, i);
		if (mkdir(s, 0777) == -1)
			return -1;
	}

	return 0;
}


/* FSCK helpers. */
static int fsck_scan_clusters(int fix)
{
	int unclean = 0;
	char s[256];
	DIR *dir;
	struct dirent *e;
	int fd;
	long name, name2;
	struct swcluster *cluster;

	/* Loop over all metas, deleting un-gettable clusters. */
	for (name2 = 0; name2 < 256; name2++) {
		snprintf(s, 255, "%s/%lX", swap.clusters_meta_base, name2);
		dir = opendir(s);
		if (!dir) {
			DPRINTF("WARNING! Missing hash directory %lX\n", name2);
			if (!fix)
				return 1;
			if (mkdir(s, 0777) == -1)
				DPRINTF("ERROR recreating hash directory %s\n", s);
			continue;
		}
		while ((e = readdir(dir))) {
			if (sscanf(e->d_name, "%lX", &name) != 1) {
				if (strcmp(e->d_name, ".") != 0
				    && strcmp(e->d_name, "..") != 0) {
					DPRINTF("WARNING! Strange file %s in clusters meta directory.\n", e->d_name);
				}
				continue;
			}
			name = name2 | (name << 8);

			cluster = cluster_get(name, CLUSTERGET_READFILES, -1);
			if (cluster) {
				cluster_put(cluster, 0);
				continue;
			}
			/* Unclean - remove stale cluster, if fix is set. */
			if (!fix) {
				closedir(dir);
				return 1;
			}
			unclean = 1;
			DPRINTF("Deleting stale cluster %lX\n", name);
			snprintf(s, 255, "%s/%lX/%lX", swap.clusters_meta_base,
				 name & 0xff, name >> 8);
			unlink(s);
			snprintf(s, 255, "%s/%lX/%lX", swap.clusters_data_base,
				 name & 0xff, name >> 8);
			unlink(s);

		}
		closedir(dir);
	}

	/* Loop over all datas, deleting un-gettable clusters, but trying
	 * to fix missing meta. */
	for (name2 = 0; name2 < 256; name2++) {
		snprintf(s, 255, "%s/%lX", swap.clusters_data_base, name2);
		dir = opendir(s);
		if (!dir) {
			DPRINTF("WARNING! Missing hash directory %lX\n", name2);
			if (!fix)
				return 1;
			if (mkdir(s, 0777) == -1)
				DPRINTF("ERROR recreating hash directory %s\n", s);
			continue;
		}
		while ((e = readdir(dir))) {
			if (sscanf(e->d_name, "%lX", &name) != 1) {
				if (strcmp(e->d_name, ".") != 0
				    && strcmp(e->d_name, "..") != 0) {
					DPRINTF("WARNING! Strange file %s in clusters meta directory.\n", e->d_name);
				}
				continue;
			}
			name = name2 | (name << 8);

			/* Create the meta, if not exisiting. */
			snprintf(s, 255, "%s/%lX/%lX", swap.clusters_meta_base,
				 name & 0xff, name >> 8);
			if (access(s, R_OK) == -1) {
				/* Unclean - try to fix, if fix is set. */
				if (!fix) {
					closedir(dir);
					return 1;
				}
				unclean = 1;
				if ((fd = open(s, O_RDWR|O_CREAT, 0666)) != -1) {
					DPRINTF("Recreating missing metadata file for cluster %lX\n", name);
				/* We need to add a dummy reference to prevent
				 * cluster from being deleted. */
					cluster = cluster_get(name, CLUSTERGET_READFILES, -1);
					if (cluster) {
						cluster_addfileref(cluster, 1);
						cluster_put(cluster, 0);
					}
					close(fd);
				} else
					DPRINTF("FAILED recreating metadata!\n");
			}
			cluster = cluster_get(name, CLUSTERGET_READFILES, -1);
			if (cluster) {
				cluster_put(cluster, 0);
				continue;
			}
			/* Unclean - remove stale cluster, if fix is set. */
			if (!fix) {
				closedir(dir);
				return 1;
			}
			unclean = 1;
			DPRINTF("Deleting stale cluster %lX\n", name);
			unlink(s);
			snprintf(s, 255, "%s/%lX/%lX", swap.clusters_data_base,
				 name & 0xff, name >> 8);
			unlink(s);

		}
		closedir(dir);
	}

	return unclean;
}
static int fsck_file(struct swfile *file, int fix)
{
	struct swcluster *cluster;
	int i, j, cnt, unclean = 0;

	/* Check internal ctree consistency. */
	if (ctree_check(file->clusters) == -1) {
		/* If we're not allowed to fix the tree - we're
		 * screwed - just bail out in this case. */
		if (!fix)
			return 1;

		/* We're going to force a rebuild. */
		unclean = 1;
		DPRINTF("File %lX has inconsistent ctree - rebuilding\n", file->name);
		ctree_build(file->clusters);
		file->flags |= SWF_DIRTY;
	}

	/* Get all clusters and check file reference and size. */
	for (i=0; i<file->clusters->cnt; i++) {
		/* Get the cluster. */
		cluster = cluster_get(CID(file->clusters, i),
				      CLUSTERGET_READFILES, -1);

		/* Not existing cluster? -> remove it from ctree. */
		if (cluster)
			goto check_size;
		if (!fix)
			return 1;
		unclean = 1;
		DPRINTF("Removing not exisiting cluster %lX from ctree of file %lX\n", (long)CID(file->clusters, i), file->name);
		file->clusters = ctree_remove(file->clusters, i, 1,
					      NULL, NULL);
		file->flags |= SWF_DIRTY;
		i--;
		continue;

	check_size:
		/* Non-matching size. Fix the ctree. */
		if (cluster->size == CSIZE(file->clusters, i))
			goto check_references;
		if (!fix)
			return 1;
		unclean = 1;
		DPRINTF("Fixing ctree cluster size at pos %i from %li to %li\n", i, (long)CSIZE(file->clusters, i), (long)cluster->size);
		ctree_replace1(file->clusters, i,
			       CID(file->clusters, i), cluster->size);
		file->flags |= SWF_DIRTY;

	check_references:
		/* Check we have as much references on the cluster
		 * as we need. */
		cnt = 0;
		for (j=0; j<file->clusters->cnt; j++)
			if (CID(file->clusters, j) == cluster->name)
				cnt++;
		for (j=0; j<cluster->files_cnt; j++)
			if (cluster->files[j] == file->name)
				cnt--;
		if (cnt == 0)
			goto next_cluster;
		if (!fix)
			return 1;
		unclean = 1;
		DPRINTF("Fixing incorrect number of references %lX -> %lX (off by %i)\n", file->name, cluster->name, cnt);
		for (; cnt<0; ++cnt)
			cluster_delfileref(cluster, file->name);
		for (; cnt>0; cnt--)
			cluster_addfileref(cluster, file->name);

	next_cluster:
		cluster_put(cluster, 0);
	}

	return unclean;
}
static int fsck_check_files(int fix)
{
	SWDIR *dir;
	long nm;
	char s[256];
	int unclean = 0;

	/* Loop over all files and try to "touch" them. */
	dir = sw_opendir();
	while ((nm = sw_readdir(dir)) != -1) {
		struct swfile *file;

		/* Get the file. */
		file = file_get(nm, FILEGET_READCLUSTERS);
		if (!file || (file->flags & SWF_NOT_IN_CORE)) {
			if (file)
				file_put(file, 0);
			if (!fix)
				return 1;
			DPRINTF("Deleting stale file %lX\n", nm);
			snprintf(s, 255, "%s/%lX", swap.files_base, nm);
			unlink(s);
			unclean = 1;
			continue;
		}

		/* Check it. */
		unclean |= fsck_file(file, 1);
		if (unclean && !fix)
			return 1;

		/* Finished with the file. */
		file_put(file, 0);
	}
	sw_closedir(dir);

	return unclean;
}
static int fsck_cluster(struct swcluster *cluster, int fix)
{
	struct swfile *file;
	int unclean = 0, i, j;

 check_again:
	for (i=0; i<cluster->files_cnt; i++) {
		/* Get the file referenced. */
		file = file_get(cluster->files[i], FILEGET_READCLUSTERS);

		/* If the file referenced does not exist - delete
		 * the reference. */
		if (!file)
			goto delete_ref;

		/* Check, the reference is valid, i.e. the file actually
		 * contains the cluster. */
		for (j=0; j<file->clusters->cnt; j++) {
			if (CID(file->clusters, j) == cluster->name) {
				file_put(file, 0);
				goto next;
			}
		}
		file_put(file, 0);

		/* Ok, bogous reference - kill it, if requested. */
	delete_ref:
		if (!fix)
			return 1;
		DPRINTF("Deleting stale fileref %lX -> %lX\n",
			cluster->name, cluster->files[i]);
		cluster_delfileref(cluster, cluster->files[i]);
		unclean = 1;

		/* As we changed the cluster->files[] array, we need to
		 * redo the checking. */
		goto check_again;
	next:
		;
	}

	return unclean;
}
static int fsck_check_clusters(int fix)
{
	DIR *dir;
	struct dirent *e;
	int unclean = 0;
	long name, name2;
	char s[256];
	struct swcluster *cluster;
	struct swfile *file = NULL;

	/* Loop over all clusters, checking files & references. */
	for (name2 = 0; name2 < 256; name2++) {
		snprintf(s, 255, "%s/%lX", swap.clusters_meta_base, name2);
		dir = opendir(s);
		while ((e = readdir(dir))) {
			if (sscanf(e->d_name, "%lX", &name) != 1)
				continue;
			name = name2 | (name << 8);
			cluster = cluster_get(name, CLUSTERGET_READFILES, -1);
			if (!cluster) {
				DPRINTF("Cannot get cluster %lX?\n", name);
				continue; /* Huh?? */
			}
			
			/* Fix the cluster. */
			unclean |= fsck_cluster(cluster, fix);
			if (unclean && !fix)
				return 1;

			/* If the cluster has no references left attach it to
			 * the lost-and-found file. Kill non-SAMPLE sized
			 * clusters. */
			if (cluster->files_cnt == 0) {
				if (!fix)
					return 1;
				if (cluster->size & (SAMPLE_SIZE-1)) {
					DPRINTF("Killing odd-sized (%li) cluster %lX\n", (long)cluster->size, name);
					cluster_put(cluster, CLUSTERPUT_FREE);
					goto out_freed;
				}
				if (!file) {
					long name;
					while ((file = file_get((name = rand()), 0)))
						file_put(file, 0);
					file = file_get(name, FILEGET_CREAT|FILEGET_READCLUSTERS);
				}
				file->clusters = ctree_insert1(file->clusters, file->clusters->cnt,
							       cluster->name, cluster->size);
				file->flags |= SWF_DIRTY;
				cluster_addfileref(cluster, file->name);
				unclean = 1;
			}
			cluster_put(cluster, 0);
		out_freed:
			unclean = 1;
		}
		closedir(dir);
	}
	if (file)
		file_put(file, FILEPUT_SYNC);

	return unclean;
}

/* Tries to recover from an "unclean shutdown". */
int swapfile_fsck(const char *name, int force)
{
	int unclean;

	if (SWAPFILE_OK()) {
		errno = EINVAL;
		return -1;
	}

	/* Can we open the swapfile w/o check? */
	if (_swapfile_init(name, 0) == 0) {
		swapfile_close();
		if (!force)
			return 0;
	}

	/* Force init of the swapfile. */
	if (_swapfile_init(name, 1) == -1)
		return -1;

	/* We are now ready for a few checks of the filesystems
	 * integrity. */
	swap.fsck = 1;
	unclean = 0;

	/* Loop over all cluster data/meta files and check if they
	 * are valid clusters. Try to fix clusters without metadata. */
	unclean |= fsck_scan_clusters(1);

	/* Loop over all files and try to "touch" them. */
	unclean |= fsck_check_files(1);

	/* Loop over all clusters and remove stale references to
	 * non-existing files and wrong references. */
	unclean |= fsck_check_clusters(1);

	/* Force deletion of .lock and close the swap. */
	swap.clean = 1;
	swapfile_close();

	return unclean;
}


/**********************************************************************
 * Operations on the swapfile namespace. Unlike a unix filesystem
 * the swapfile filesystem has names composed out of a single "long".
 * Also the swapfile name hierarchy is flat - i.e. no directories.
 * Names are >=0, negative values are reserved.
 * All namespace operations are atomic (i.e. thread safe) and not
 * undoable (well - just sw_unlink is not undoable).
 */

/* Deletes a name from the filesystem. Like unlink(2). */
int sw_unlink(long name)
{
	struct swfile *f;

	if (!SWAPFILE_RW()) {
		errno = EINVAL;
		return -1;
	}

	/* No locking needed!? */
	if (!(f = file_get(name, FILEGET_READCLUSTERS)))
		return -1;
	file_put(f, FILEPUT_UNLINK);

	return 0;
}


/* Open the (flat) swapfile directory for reading. The stream
 * is positioned at the first file. Like opendir(3), but w/o
 * directory specification for obvious reason. */
SWDIR *sw_opendir()
{
	if (!SWAPFILE_OK()) {
		errno = EINVAL;
		return NULL;
	}

	return (SWDIR *)opendir(swap.files_base);
}

/* As the namespace is rather simple the equivalent to readdir(3) is
 * just returning the names, no directory entry. Anything else
 * is like readdir(3). If no further entries are available, -1 is returned. */
long sw_readdir(SWDIR *d)
{
    	struct dirent *e;
	long name;

	if (!SWAPFILE_OK() || !d) {
		errno = EINVAL;
		return -1;
	}

	while ((e = readdir((DIR *)d))) {
		if (strcmp(e->d_name, "clusters.meta") == 0
		    || strcmp(e->d_name, "clusters.data") == 0)
			continue;
		if (sscanf(e->d_name, "%lX", &name) == 1)
		    	return name;
	}
	return -1;
}

/* Like closedir(3). */
int sw_closedir(SWDIR *d)
{
	if (!SWAPFILE_OK() || !d) {
		errno = EINVAL;
		return -1;
	}

    	return closedir((DIR *)d);
}

/**********************************************************************
 * Operations on a single file. Files are organized in variable sized
 * clusters. Access to the file is limited to mapping those clusters.
 */

/* Open a file like open(2) - flags can be O_CREAT, O_EXCL,
 * O_RDWR, O_RDONLY, O_WRONLY with same semantics as open(2).
 * Returns a file descriptor on success, -1 on error.
 * The optional transaction id is used for all operations
 * operating on the swfd_t, they can be undone and redone
 * this way. */
swfd_t sw_open(long name, int flags)
{
	struct swfile *f;
	struct swfd *fd;

	if (!SWAPFILE_OK()) {
		errno = EINVAL;
		return -1;
	}

	/* Detect illegal/unsupported flag combinations. */
	if (((flags & O_EXCL) && !(flags & O_CREAT))
	    || (flags & ~(O_CREAT|O_EXCL|O_TRUNC|O_RDONLY|O_WRONLY|O_RDWR))) {
		errno = EINVAL;
		return -1;
	}

	/* Alloc fd. */
	if (!(fd = (struct swfd *)malloc(sizeof(struct swfd))))
		return -1;
	if (hash_find_swfd((((long)fd)>>2)))
		DERROR("swfd_t hash clash!??");

	LOCK;
	f = file_get(name, FILEGET_READCLUSTERS);

	/* File is unlinked. */
	if (f && (f->flags & SWF_UNLINKED)) {
		if (flags & O_CREAT)
			errno = EAGAIN;
		else
			errno = ENOENT;
		goto err;
	}

	/* File exists and is requested to be created exclusively? */
	if (f && (flags & O_EXCL)) {
		errno = EEXIST;
		goto err;
	}
	/* FIle does not exist and is not requested to being created? */
	if (!f && !(flags & O_CREAT)) {
		errno = ENOENT;
		goto err;
	}

	/* Create file, if necessary and requested. */
	if (!f && (flags & O_CREAT)) {
		if (!(f = file_get(name, FILEGET_CREAT|FILEGET_READCLUSTERS)))
			goto err; /* FIXME: which errno? */
	}
	/* Truncate file, if requested. */
	if (flags & O_TRUNC)
		file_truncate(f, 0);

	GLAME_INIT_LIST_HEAD(&fd->list);
	hash_init_swfd(fd);
	fd->file = f;
	fd->fd = ((long)fd)>>2;
	fd->mode = flags & (O_RDONLY|O_WRONLY|O_RDWR);
	fd->offset = 0;
	glame_list_add_swfd(fd);
	hash_add_swfd(fd);
	UNLOCK;

	errno = 0;
	return fd->fd;

 err:
	if (f)
		file_put(f, 0);
	UNLOCK;
	free(fd);
	return -1;
}

/* Closes a file descriptor. Like close(2). */
int sw_close(swfd_t fd)
{
	struct swfd *_fd;

	if (!SWAPFILE_OK()) {
		errno = EINVAL;
		return -1;
	}

	LOCK;
	if (!(_fd = hash_find_swfd(fd))) {
		UNLOCK;
		errno = EINVAL;
		return -1;
	}
	hash_remove_swfd(_fd);
	glame_list_del_swfd(_fd);
	UNLOCK;
	file_put(_fd->file, 0);
	free(_fd);

	return 0;
}

/* Changes the size of the file fd like ftruncate(2). */
int sw_ftruncate(swfd_t fd, off_t length)
{
	struct swfd *_fd;
	int res;

	if (!SWAPFILE_RW()) {
		errno = EINVAL;
		return -1;
	}

	LOCK;
	if (!(_fd = hash_find_swfd(fd))) {
		UNLOCK;
		errno = EINVAL;
		return -1;
	}
	UNLOCK;

	/* Check if its a valid truncate command. */
	if (_fd->mode == O_RDONLY) {
		errno = EPERM;
		return -1;
	}
	if (length < 0) {
		errno = EINVAL;
		return -1;
	}

	/* Global truncate lock, as we do interesting things inside
	 * file_truncate. */
	LOCK;
	res = file_truncate(_fd->file, length);
	UNLOCK;
	if (res == -1)
		return -1;

	return 0;
}

/* Tries to copy count bytes from the current position of in_fd
 * to the current position of out_fd (updating both file pointer
 * positions). The actual number of copied bytes is returned, or
 * -1 on an error.
 * Two different modes are supported (may be or'ed together):
 * - SWSENDFILE_INSERT inserts into, rather than overwrites/extends
 *   the destination file
 * - SWSENDFILE_CUT removes copied data from the source file
 * The destination file descriptor may be SW_NOFILE, in that case
 * no data is actually written (useful with SWSENDFILE_CUT). */
ssize_t sw_sendfile(swfd_t out_fd, swfd_t in_fd, size_t count, int mode)
{
	struct swfd *_ofd;
	struct swfd *_ifd;

	if (!SWAPFILE_RW()) {
		errno = EINVAL;
		return -1;
	}

	LOCK;
	_ofd = hash_find_swfd(out_fd);
	_ifd = hash_find_swfd(in_fd);
	UNLOCK;

	/* Check for read/write permissions. */
	errno = EPERM;
	if ((_ofd && !(_ofd->mode == O_WRONLY || _ofd->mode == O_RDWR))
	    || (_ifd && !(_ifd->mode == O_RDONLY || _ifd->mode == O_RDWR)))
		return -1;

	/* Check for correct mode argument. */
	errno = EINVAL;
	if (mode & ~(SWSENDFILE_INSERT|SWSENDFILE_CUT))
		return -1;

	/* We need an input file with at least count bytes data ready
	 * to be read from. */
	if (!_ifd || _ifd->file->clusters->size - _ifd->offset < count)
		return -1;

#if 0
	/* We need an output file with bytes to be overridden already
	 * there (for ~SWSENDFILE_INSERT mode). */
	if (!(mode & SWSENDFILE_INSERT)
	    && _ofd && _ofd->offset+count > _ofd->file->clusters->size)
	        return -1;
#endif

	/* If source and destination file are the same we do not
	 * allow overlapping areas as we do not handle them correctly. */
	if (_ifd && _ofd && _ifd->file == _ofd->file
	    && ((_ifd->offset < _ofd->offset+count
		 && _ifd->offset >= _ofd->offset)
		|| (_ofd->offset < _ifd->offset+count
		    && _ofd->offset >= _ifd->offset)))
		return -1;
	errno = 0;

	/* If count is zero just return successfully. */
	if (count == 0)
		return 0;

	/* To be able to easy seperate the operation, we need to
	 * do things in a correct order.
	 */
	/* First update the output file, if necessary. */
	if (_ofd) {
		if (mode & SWSENDFILE_INSERT) {
			if (_ofd->file->clusters->size < _ofd->offset) {
				LOCK;
				file_truncate(_ofd->file, _ofd->offset);
				UNLOCK;
			}
			file_insert(_ofd->file, _ofd->offset,
				    _ifd->file, _ifd->offset, count);
		} else {
			if (_ofd->file->clusters->size < _ofd->offset + count) {
				LOCK;
				file_truncate(_ofd->file, _ofd->offset + count);
				UNLOCK;
			}
			file_replace(_ofd->file, _ofd->offset,
				     _ifd->file, _ifd->offset, count);
		}
		_ofd->offset = _ofd->offset + count;
	}
	/* Then update the input file, if necessary */
	if (mode & SWSENDFILE_CUT)
		file_cut(_ifd->file, _ifd->offset, count);
	else
		_ifd->offset += count;

	return 0;
}

/* Update the file pointer position like lseek(2). */
off_t sw_lseek(swfd_t fd, off_t offset, int whence)
{
	struct swfd *_fd;

	if (!SWAPFILE_OK()) {
		errno = EINVAL;
		return -1;
	}

	LOCK;
	if (!(_fd = hash_find_swfd(fd))) {
		UNLOCK;
		errno = EINVAL;
		return -1;
	}
	UNLOCK;

	/* We convert all seeks to SEEK_SET seeks which can be
	 * handled by common code. */
	switch (whence) {
	case SEEK_END:
		offset = _fd->file->clusters->size + offset;
		break;
	case SEEK_CUR:
		offset += _fd->offset;
		break;
	case SEEK_SET:
		break;
	default:
		errno = EINVAL;
		return -1;
	}
	if (offset < 0) {
		errno = EINVAL;
		return -1;
	}
	_fd->offset = offset;

	return _fd->offset;
}

#ifdef USE_COOKED_OPS_READ
/* Like read(2), read count bytes from the current filepointer
 * position to the array pointed to by buf. */
ssize_t sw_read(swfd_t fd, void *buf, size_t count)
{
	struct sw_stat stat;
	char *mem;
	size_t dcnt, cnt;
	int err = 0;

	if (!SWAPFILE_OK()) {
		errno = EINVAL;
		return -1;
	}

	/* Check, if we will cross the file end and correct
	 * the count appropriately. */
	if (sw_fstat(fd, &stat) == -1)
		return -1;
	if (stat.offset + count > stat.size)
		count -= stat.offset + count - stat.size;

	cnt = count;
	while (cnt > 0) {
		if (sw_fstat(fd, &stat) == -1
		    || (mem = (char *)sw_mmap(NULL, PROT_READ,
					      MAP_SHARED, fd)) == MAP_FAILED) {
			err = 1;
			break;
		}

		dcnt = MIN(stat.cluster_size
			   - (stat.offset - stat.cluster_start), cnt);
		memcpy(&((char *)buf)[count-cnt],
		       &mem[stat.offset - stat.cluster_start], dcnt);

		if (sw_munmap(mem) == -1
		    || sw_lseek(fd, dcnt, SEEK_CUR) == -1) {
			err = 1;
			break;
		}

		cnt -= dcnt;
	}

	if (err && (count-cnt == 0))
		return -1;
	return count-cnt;
}
#else
/* Like read(2), read count bytes from the current filepointer
 * position to the array pointed to by buf. */
ssize_t sw_read(swfd_t fd, void *buf, size_t count)
{
	size_t dcnt, cnt;
	struct swfd *_fd;
	struct swcluster *c;
	s64 coff;
	ssize_t res;
	int err = 0;

	if (!SWAPFILE_OK()) {
		errno = EINVAL;
		return -1;
	}

	LOCK;
	if (!(_fd = hash_find_swfd(fd)) || !buf || count<0) {
		UNLOCK;
		errno = EINVAL;
		return -1;
	}
	UNLOCK;

	/* Check, if we will cross the file end and correct
	 * the count appropriately. */
	if (_fd->file->clusters->size < _fd->offset + count)
		count -= _fd->offset + count - _fd->file->clusters->size;

	cnt = count;
	while (cnt > 0) {
		if (!(c = file_getcluster(_fd->file, _fd->offset, &coff, 0))) {
			err = 1;
			break;
		}

		dcnt = MIN(c->size - (_fd->offset - coff), cnt);
		if ((res = cluster_read(c, &((char *)buf)[count-cnt], dcnt,
					_fd->offset - coff)) != dcnt) {
			if (res == -1)
				err = 1;
			else
				cnt -= res;
			cluster_put(c, 0);
			break;
		}

		cluster_put(c, 0);
		_fd->offset += dcnt;
		cnt -= dcnt;
	}

	if (err && (count-cnt == 0))
		return -1;
	return count-cnt;
}
#endif

#ifdef USE_COOKED_OPS_WRITE
/* Like write(2), write count bytes from buf starting at the current
 * filepointer position. */
ssize_t sw_write(swfd_t fd, const void *buf, size_t count)
{
	struct sw_stat stat;
	char *mem;
	size_t dcnt, cnt = count;
	s64 old_size = -1, old_offset;
	int err = 0;

	if (!SWAPFILE_RW()) {
		errno = EINVAL;
		return -1;
	}

	/* Check, if we need to expand the file. */
	if (sw_fstat(fd, &stat) == -1)
		return -1;
	if (stat.offset + count > stat.size) {
		old_size = stat.size;
		old_offset = stat.offset;
		if (sw_ftruncate(fd, stat.offset + count) == -1)
			return -1;
	}

	while (cnt > 0) {
		if (sw_fstat(fd, &stat) == -1
		    || (mem = (char *)sw_mmap(NULL, PROT_WRITE,
					      MAP_SHARED, fd)) == MAP_FAILED) {
			err = 1;
			break;
		}

		dcnt = MIN(stat.cluster_size
			   - (stat.offset - stat.cluster_start), cnt);
		memcpy(&mem[stat.offset - stat.cluster_start],
		       &((const char *)buf)[count-cnt], dcnt);

		if (sw_munmap(mem) == -1
		    || sw_lseek(fd, dcnt, SEEK_CUR) == -1) {
			err = 1;
			break;
		}

		cnt -= dcnt;
	}

	/* Did we have to truncate the file and were not be able
	 * to write all data? We may have to fix the truncation here. */
	if (cnt != 0 && old_size != -1) {
		if (old_offset + (count-cnt) > old_size)
			sw_ftruncate(fd, old_offset + (count-cnt));
	}

	if (err && (count-cnt == 0))
		return -1;
	return count-cnt;
}
#else
/* Like write(2), write count bytes from buf starting at the current
 * filepointer position. */
ssize_t sw_write(swfd_t fd, const void *buf, size_t count)
{
	size_t dcnt, cnt = count;
	struct swfd *_fd;
	struct swcluster *c;
	s64 coff, old_size = -1, old_offset = -1;
	ssize_t res;
	int err = 0;

	if (!SWAPFILE_RW()) {
		errno = EINVAL;
		return -1;
	}

	LOCK;
	if (!(_fd = hash_find_swfd(fd)) || !buf || count<0) {
		UNLOCK;
		errno = EINVAL;
		return -1;
	}
	UNLOCK;

	/* Check, if we need to expand the file. */
	if (_fd->offset + count > _fd->file->clusters->size) {
		old_size = _fd->file->clusters->size;
		old_offset = _fd->offset;
		LOCK;
		err = file_truncate(_fd->file, _fd->offset + count);
		UNLOCK;
		if (err == -1)
			return -1;
	}

	while (cnt > 0) {
		if (!(c = file_getcluster_private(_fd->file, _fd->offset, &coff, 0))) {
			err = 1;
			break;
		}

		dcnt = MIN(c->size - (_fd->offset - coff), cnt);
		if ((res = cluster_write(c, &((char *)buf)[count-cnt], dcnt,
					 _fd->offset - coff)) != dcnt) {
			if (res == -1)
				err = 1;
			else
				cnt -= res;
			cluster_put(c, 0);
			break;
		}

		cluster_put(c, 0);
		_fd->offset += dcnt;
		cnt -= dcnt;
	}

	/* Did we have to truncate the file and were not be able
	 * to write all data? We may have to fix the truncation here. */
	if (cnt != 0 && old_size != -1) {
		if (old_offset + (count-cnt) > old_size) {
			LOCK;
			file_truncate(_fd->file, old_offset + (count-cnt));
			UNLOCK;
		}
	}

	if (err && (count-cnt == 0))
		return -1;
	return count-cnt;
}
#endif

/* Obtain information about the file - works like fstat(2), but
 * with different struct stat. Also included is information about
 * the actual (file pointer position, see sw_lseek) cluster which
 * can be mapped using sw_mmap. */
int sw_fstat(swfd_t fd, struct sw_stat *buf)
{
	struct swfd *_fd;
	struct swcluster *c;
	s64 coff;

	if (!SWAPFILE_OK() || !buf) {
		errno = EINVAL;
		return -1;
	}

	LOCK;
	if (!(_fd = hash_find_swfd(fd))) {
		UNLOCK;
		errno = EINVAL;
		return -1;
	}
	UNLOCK;

	buf->name = _fd->file->name;
	buf->size = _fd->file->clusters->size;
	buf->mode = _fd->mode;
	buf->offset = _fd->offset;
	if (_fd->offset < _fd->file->clusters->size) {
	        c = file_getcluster(_fd->file, _fd->offset, &coff, 0);
		buf->cluster_start = coff;
		buf->cluster_end = coff + c->size - 1;
		buf->cluster_size = c->size;
		cluster_put(c, 0);
	} else {
		/* FIXME!? Memory mapping of this cluster is not allowed,
		 * but writing via sw_write is ok. */
		buf->cluster_start = _fd->offset;
		buf->cluster_end = _fd->offset - 1;
		buf->cluster_size = 0;
	}

	return 0;
}

/* Maps the actual (file pointer position, see sw_lseek and sw_fstat)
 * cluster into memory with parameters like mmap(2) - no size/offset
 * as they are determined by the actual cluster offset/size. */
void *sw_mmap(void *start, int prot, int flags, swfd_t fd)
{
	struct swfd *_fd;
	struct swcluster *c;
	s64 coff;
	void *addr;

	if (!SWAPFILE_RW()) {
		errno = EINVAL;
		return MAP_FAILED;
	}

	if (flags & ~(MAP_PRIVATE|MAP_SHARED)) {
		errno = EINVAL;
		return MAP_FAILED;
	}

	LOCK;
	if (!(_fd = hash_find_swfd(fd))) {
		UNLOCK;
		errno = EINVAL;
		return MAP_FAILED;
	}
	UNLOCK;

	/* Check file permissions. */
	if (!(_fd->mode == O_RDWR)
	    && (((prot & PROT_WRITE) && !(_fd->mode == O_WRONLY))
		|| ((prot & PROT_READ) && !(_fd->mode == O_RDONLY)))) {
		errno = EPERM;
		return MAP_FAILED;
	}

	/* Get the actual cluster and mmap it, but replace it with a
	 * copy, if PROT_WRITE is set and the cluster is shared. */
	if (prot & PROT_WRITE)
		c = file_getcluster_private(_fd->file, _fd->offset, &coff, 0);
	else
		c = file_getcluster(_fd->file, _fd->offset, &coff, 0);
	if (c) {
		addr = cluster_mmap(c, prot, flags);
		cluster_put(c, 0);
		return addr;
	}

	/* Memory mapping after the end of the file is not going to work
	 * just like for normal files. */
	errno = EINVAL;
	return MAP_FAILED;
}

/* Unmaps a previously mapped part of a file. Like munmap(2). */
int sw_munmap(void *start)
{
	if (!SWAPFILE_OK()) {
		errno = EINVAL;
		return -1;
	}

	return cluster_munmap((char *)start);
}

