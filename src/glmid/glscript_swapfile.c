/*
 * glscript_swapfile.c
 *
 * Copyright (C) 2000 Richard Guenther
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

#include <guile/gh.h>
#include <swapfile.h>


#define gh_scm2swfd(s) (swfd_t)gh_scm2long(s)
#define gh_scm2txnid(t) (txnid_t)gh_scm2long(t)
#define gh_txnid2scm(t) gh_long2scm((long)(t))


/* The scriptable txn API part.
 */

static SCM gls_txn_start(SCM s_tid)
{
	txnid_t tid;
	if (!gh_exact_p(s_tid))
		return SCM_BOOL_F; /* SCM_ERR? */
	tid = txn_start(gh_scm2txnid(s_tid));
	if (tid == -1)
		return SCM_BOOL_F;
	return gh_txnid2scm(tid);
}

static SCM gls_txn_end(SCM s_tid)
{
	if (!gh_exact_p(s_tid))
		return SCM_BOOL_F; /* SCM_ERR? */
	if (txn_end(gh_scm2txnid(s_tid)) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_txn_abort(SCM s_tid)
{
	if (!gh_exact_p(s_tid))
		return SCM_BOOL_F; /* SCM_ERR? */
	if (txn_abort(gh_scm2txnid(s_tid)) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_txn_undo(SCM s_tid)
{
	txnid_t tid;
	if (!gh_exact_p(s_tid))
		return SCM_BOOL_F; /* SCM_ERR? */
	tid = txn_undo(gh_scm2txnid(s_tid));
	if (tid == -1)
		return SCM_BOOL_F;
	return gh_txnid2scm(tid);
}

static SCM gls_txn_delete(SCM s_tid)
{
	if (!gh_exact_p(s_tid))
		return SCM_BOOL_F; /* SCM_ERR? */
	if (txn_delete(gh_scm2txnid(s_tid)) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}


/* The scriptable swapfile API part.
 * No gls_sw_mmap, gls_sw_munmap.
 */

static SCM gls_swapfile_open(SCM s_name)
{
	char *name;
	int namel, res;

	if (!gh_string_p(s_name))
		return SCM_BOOL_F; /* SCM_ERR? */
	name = gh_scm2newstr(s_name, &namel);
	res = swapfile_open(name, 0);
	free(name);
	if (res == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_swapfile_close()
{
	swapfile_close();
	return SCM_BOOL_T;
}

static SCM gls_swapfile_creat(SCM s_name, SCM s_size)
{
	char *name;
	int namel, res;

	if (!gh_string_p(s_name) || !gh_exact_p(s_size))
		return SCM_BOOL_F; /* SCM_ERR? */
	name = gh_scm2newstr(s_name, &namel);
	res = swapfile_creat(name, gh_scm2long(s_size));
	free(name);
	if (res == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_sw_unlink(SCM s_name)
{
	if (!gh_exact_p(s_name))
		return SCM_BOOL_F; /* SCM_ERR? */
	if (sw_unlink(gh_scm2long(s_name)) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_sw_opendir()
{
	return gh_long2scm((long)sw_opendir());
}

static SCM gls_sw_readdir(SCM s_d)
{
	if (!gh_exact_p(s_d))
		return SCM_BOOL_F; /* SCM_ERR */
	return gh_long2scm((long)sw_readdir((SWDIR *)gh_scm2long(s_d)));
}

static SCM gls_sw_closedir(SCM s_d)
{
	if (!gh_exact_p(s_d))
		return SCM_BOOL_F; /* SCM_ERR */
	sw_closedir((SWDIR *)gh_scm2long(s_d));
	return SCM_BOOL_T;
}

static SCM gls_sw_open(SCM s_name, SCM s_flags, SCM s_tid)
{
	swfd_t fd;

	if (!gh_exact_p(s_name) || !gh_exact_p(s_flags)
	    || !gh_exact_p(s_tid))
		return SCM_BOOL_F; /* SCM_ERR */
	fd = sw_open(gh_scm2long(s_name), gh_scm2long(s_flags),
		     gh_scm2long(s_tid));
	if (fd == -1)
		return SCM_BOOL_F;
	return gh_long2scm((long)fd);
}

static SCM gls_sw_close(SCM s_fd)
{
	if (!gh_exact_p(s_fd))
		return SCM_BOOL_F; /* SCM_ERR */
	if (sw_close(gh_scm2swfd(s_fd)) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_sw_fstat(SCM s_fd)
{
	struct sw_stat st;

	if (!gh_exact_p(s_fd))
		return SCM_BOOL_F; /* SCM_ERR */
	if (sw_fstat(gh_scm2swfd(s_fd), &st) == -1)
		return SCM_BOOL_F;
	return gh_list(gh_long2scm(st.name), gh_long2scm(st.size),
		       gh_long2scm(st.mode), gh_long2scm(st.offset),
		       gh_long2scm(st.cluster_start),
		       gh_long2scm(st.cluster_end),
		       gh_long2scm(st.cluster_size), SCM_UNDEFINED);
}

static SCM gls_sw_ftruncate(SCM s_fd, SCM s_length)
{
	if (!gh_exact_p(s_fd) || !gh_exact_p(s_length))
		return SCM_BOOL_F; /* SCM_ERR */
	if (sw_ftruncate(gh_scm2swfd(s_fd), gh_scm2long(s_length)) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_sw_lseek(SCM s_fd, SCM s_offset, SCM s_whence)
{
	long res;
	if (!gh_exact_p(s_fd) || !gh_exact_p(s_offset)
	    || !gh_exact_p(s_whence))
		return SCM_BOOL_F; /* SCM_ERR */
	res = sw_lseek(gh_scm2swfd(s_fd), gh_scm2long(s_offset),
		       gh_scm2long(s_whence));
	if (res == -1)
		return SCM_BOOL_F;
	return gh_long2scm(res);
}

static SCM gls_sw_sendfile(SCM s_out_fd, SCM s_in_fd, SCM s_count, SCM s_mode)
{
	if (!gh_exact_p(s_out_fd) || !gh_exact_p(s_in_fd)
	    || !gh_exact_p(s_count) || !gh_exact_p(s_mode))
		return SCM_BOOL_F; /* SCM_ERR */
	if (sw_sendfile(gh_scm2swfd(s_out_fd), gh_scm2swfd(s_in_fd),
			gh_scm2long(s_count), gh_scm2long(s_mode)) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_sw_read_floatvec(SCM s_fd, SCM s_length)
{
	long length;
	float *m;
	SCM s_vec;

	if (!gh_exact_p(s_fd) || !gh_exact_p(s_length))
		return SCM_BOOL_F; /* SCM_ERR */
	length = gh_scm2long(s_length);
	m = (float *)malloc(length*sizeof(float));
	if (sw_read(gh_scm2swfd(s_fd), m,
		    length*sizeof(float)) != length*sizeof(float)) {
		free(m);
		return SCM_BOOL_F;
	}
	s_vec = gh_floats2fvect(m, length);
	free(m); /* FIXME!?? */
	return s_vec;
}

static SCM gls_sw_read_string(SCM s_fd, SCM s_length)
{
	long length, res;
	char *m;

	if (!gh_exact_p(s_fd) || !gh_exact_p(s_length))
		return SCM_BOOL_F; /* SCM_ERR */
	length = gh_scm2long(s_length);
	m = (char *)malloc(length+1);
	if ((res = sw_read(gh_scm2swfd(s_fd), m, length)) != length) {
		free(m);
		return SCM_BOOL_F;
	}
	m[length] = '\0';
	return gh_str02scm(m);
}

static SCM gls_sw_write(SCM s_fd, SCM s_buf)
{
	long res;

	if (!gh_exact_p(s_fd))
		return SCM_BOOL_F; /* SCM_ERR */
	if (gh_vector_p(s_buf)) {
		float *fvec;
		long length;
		length = gh_vector_length(s_buf);
		fvec = (float *)malloc(length*sizeof(float));
		gh_scm2floats(s_buf, fvec); /* FIXME!? */
		res = sw_write(gh_scm2swfd(s_fd), fvec, length*sizeof(float));
		free(fvec);
		if (res != -1)
			res /= sizeof(float);
	} else if (gh_string_p(s_buf)) {
		char *str;
		int strlen;
		str = gh_scm2newstr(s_buf, &strlen);
		res = sw_write(gh_scm2swfd(s_fd), str, strlen);
		free(str);
	} else
		return SCM_BOOL_F; /* SCM_ERR */
	if (res == -1)
		return SCM_BOOL_F;
	return gh_long2scm(res);
}


static void glgh_define(const char *token, int val)
{
	char str[256];

	snprintf(str, 255, "(define %s %i)", token, val);
	gh_eval_str(str);
}

int glscript_init_swapfile()
{
	/* Transaction subsystem procedures. */
	gh_new_procedure("txn_start",
			 (SCM (*)())gls_txn_start, 1, 0, 0);
	gh_new_procedure("txn_end",
			 (SCM (*)())gls_txn_end, 1, 0, 0);
	gh_new_procedure("txn_abort",
			 (SCM (*)())gls_txn_abort, 1, 0, 0);
	gh_new_procedure("txn_undo",
			 (SCM (*)())gls_txn_undo, 1, 0, 0);
	gh_new_procedure("txn_delete",
			 (SCM (*)())gls_txn_delete, 1, 0, 0);
	glgh_define("TXN_NONE", TXN_NONE);

	/* Swapfile subsystem procedures. */
	gh_new_procedure("swapfile_open",
			 (SCM (*)())gls_swapfile_open, 1, 0, 0);
	gh_new_procedure("swapfile_close",
			 (SCM (*)())gls_swapfile_close, 0, 0, 0);
	gh_new_procedure("swapfile_creat",
			 (SCM (*)())gls_swapfile_creat, 2, 0, 0);

	gh_new_procedure("sw_unlink",
			 (SCM (*)())gls_sw_unlink, 1, 0, 0);

	gh_new_procedure("sw_opendir",
			 (SCM (*)())gls_sw_opendir, 0, 0, 0);
	gh_new_procedure("sw_readdir",
			 (SCM (*)())gls_sw_readdir, 1, 0, 0);
	gh_new_procedure("sw_closedir",
			 (SCM (*)())gls_sw_closedir, 1, 0, 0);

	gh_new_procedure("sw_open",
			 (SCM (*)())gls_sw_open, 3, 0, 0);
	gh_new_procedure("sw_close",
			 (SCM (*)())gls_sw_close, 1, 0, 0);
	gh_new_procedure("sw_fstat",
			 (SCM (*)())gls_sw_fstat, 1, 0, 0);
	gh_new_procedure("sw_ftruncate",
			 (SCM (*)())gls_sw_ftruncate, 2, 0, 0);
	gh_new_procedure("sw_lseek",
			 (SCM (*)())gls_sw_lseek, 3, 0, 0);
	gh_new_procedure("sw_sendfile",
			 (SCM (*)())gls_sw_sendfile, 4, 0, 0);

	gh_new_procedure("sw_read_floatvec",
			 (SCM (*)())gls_sw_read_floatvec, 2, 0, 0);
	gh_new_procedure("sw_read_string",
			 (SCM (*)())gls_sw_read_string, 2, 0, 0);
	gh_new_procedure("sw_write", (SCM (*)())gls_sw_write, 2, 0, 0);

	glgh_define("O_CREAT", O_CREAT);
	glgh_define("O_EXCL", O_EXCL);
	glgh_define("O_TRUNC", O_TRUNC);
	glgh_define("O_RDWR", O_RDWR);
	glgh_define("O_RDONLY", O_RDONLY);
	glgh_define("O_WRONLY", O_WRONLY);

	glgh_define("SWSENDFILE_INSERT", SWSENDFILE_INSERT);
	glgh_define("SWSENDFILE_CUT", SWSENDFILE_CUT);
	glgh_define("SW_NOFILE", (long)SW_NOFILE);

	glgh_define("SEEK_SET", SEEK_SET);
	glgh_define("SEEK_CUR", SEEK_CUR);
	glgh_define("SEEK_END", SEEK_END);

	return 0;
}
