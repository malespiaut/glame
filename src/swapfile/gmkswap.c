/*
 * gmkswap.c
 *
 * Copyright (C) 1999, 2000 Richard Guenther
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "swapfile.h"


static int do_open(char *name, off_t *size)
{
	struct stat sbuf;
	int fd;

	if ((fd = open(name, O_RDWR|O_CREAT, 0666)) == -1)
		return -1;
#if 0 /* flock is broken on solaris */
	if (flock(fd, LOCK_EX|LOCK_NB) == -1)
		goto _close;
#endif

	if (fstat(fd, &sbuf) == -1)
		goto _unlock;
	if (S_ISBLK(sbuf.st_mode)) {
		*size = sbuf.st_blocks*512;
	} else if (S_ISREG(sbuf.st_mode)) {
		if (*size != -1) {
			if (ftruncate(fd, *size) == -1)
				goto _unlock;
		} else {
			*size = sbuf.st_size;
		}
	} else {
		fprintf(stderr, "Unsupported file type\n");
		goto _unlock;
	}

	return fd;

 _unlock:
#if 0
	flock(fd, LOCK_SH);
 _close:
#endif
	close(fd);
	return -1;
}

static void do_close(int fd)
{
#if 0
	flock(fd, LOCK_SH);
#endif
	close(fd);
}

static int do_init(int fd, off_t size)
{
	swapd_header_t header;
	swapd_record_t record;
	off_t pos;
	int i;

	/* write header */

	memcpy(header.magic, SWAP_MAGIC, 16);
	header.u.header.size = size;
	header.u.header.data_off = 1024*1024; /* UH! FIXME! */
	header.u.header.data_size = size - header.u.header.data_off;
	header.u.header.meta_off = CLUSTER_MINSIZE;
	header.u.header.meta_size = header.u.header.data_off - header.u.header.meta_off;

	if (lseek(fd, 0, SEEK_SET) == -1)
		return -1;
	if (write(fd, &header, sizeof(header)) != sizeof(header))
		return -1;

	/* write metadata (clusters & EOF record) */
 
	if (lseek(fd, header.u.header.meta_off, SEEK_SET) == -1)
		return -1;

	memcpy(record.magic, RECORD_MAGIC, 4);

	pos = header.u.header.data_off;
	i = 0;
	while (pos < size) {
		record.type = RECORD_TYPE_CLUSTER;
		record.u.cluster.off = pos;
		record.u.cluster.size = MIN(CLUSTER_MAXSIZE, size - pos);
		record.u.cluster.refcnt = 0;
		record.u.cluster.id = i++;
		pos += record.u.cluster.size;
		if (write(fd, &record, sizeof(record)) != sizeof(record))
			return -1;
	}

	record.type = RECORD_TYPE_EOF;
	if (write(fd, &record, sizeof(record)) != sizeof(record))
		return -1;

	return 0;
}

static int test_conf(void)
{
	int align;

#ifdef _SC_MMAP_FIXED_ALIGNMENT
	align = sysconf(_SC_MMAP_FIXED_ALIGNMENT);
	if (align != 0 && align != -1) {
		if (align > CLUSTER_MINSIZE)
			goto _alignerr;
		else
			return 0;
	}
#endif
#ifdef _SC_PAGESIZE
	align = sysconf(_SC_PAGESIZE);
	if (align != 0 && align != -1) {
		if (align > CLUSTER_MINSIZE)
			goto _alignerr;
		else
			return 0;
	}
#endif

	return 0;

_alignerr:
	fprintf(stderr, "You have found an arch with too large page (%i)!\n",
		align);
	fprintf(stderr, "Go and fix swapfileI.h!\n");
	return -1;
}


int main(int argc, char **argv)
{
	char *swapname;
	off_t size;
	int fd;

	fprintf(stderr, "\n"
"    gmkswap version "VERSION", Copyright (C) 1999, 2000 Richard Guenther\n"
"    gmkswap comes with ABSOLUTELY NO WARRANTY.\n"
"    This is free software, and you are welcome to redistribute it\n"
"    under certain conditions.\n\n");

	if (argc<2 || argc>3)
		goto _usage;

	swapname = argv[1];
	size = -1;
	if (argc == 3)
		size = 1024*1024*atol(argv[2]);

	if (test_conf() == -1)
		exit(1);

	if ((fd = do_open(swapname, &size)) == -1) {
		perror("unable to open swapspace");
		exit(1);
	}

	if (size < 1024*1024*4) {
		fprintf(stderr, "You don't want to have only %li bytes for swap!\n",
			(long)size);
		do_close(fd);
		goto _usage;
	}

	fprintf(stderr, "Will create %li bytes swap in %s in 5 seconds!\n",
		(long)size, swapname);
	fprintf(stderr, "Last chance to quit (Ctrl-c is your friend)!\n");
	sleep(5);
	fprintf(stderr, "\nSetting up swap...\n");

	if (do_init(fd, size) == -1) {
		perror("Error in initting swap space");
		do_close(fd);
		goto _usage;
	}

	do_close(fd);
	fprintf(stderr, "...all done.\n");

	return 0;


 _usage:
	fprintf(stderr, "Usage: %s file [size in MB]\n", argv[0]);
	return 1;
}
