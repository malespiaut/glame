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
#include "swapfile.h"
#include "glscript.h"


#define gh_scm2swfd(s) (swfd_t)gh_scm2long(s)
#define gh_scm2txnid(t) (txnid_t)gh_scm2long(t)
#define gh_txnid2scm(t) gh_long2scm((long)(t))

/* SMOBs for SWDIR, swfd and txnid.
 */

static long swdir_smob_tag;
#define scm2swdir(s) (SWDIR *)scm2pointer(s, swdir_smob_tag)
#define swdir2scm(p) pointer2scm((void *)p, swdir_smob_tag)
#define scminvalidateswdir(s) scminvalidatepointer(s, swdir_smob_tag)
#define swdir_p(s) (SCM_NIMP(s) && SCM_CAR(s) == swdir_smob_tag)

static long swfd_smob_tag;
#define scm2swfd(s) (swfd_t)scm2long(s, swfd_smob_tag)
#define swfd2scm(p) long2scm(p, swfd_smob_tag)
#define swfd_p(s) (SCM_NIMP(s) && SCM_CAR(s) == swfd_smob_tag)

static long txnid_smob_tag;
#define scm2txnid(s) (txnid_t)scm2long(s, txnid_smob_tag)
#define txnid2scm(p) long2scm(p, txnid_smob_tag)
#define txnid_p(s) (SCM_NIMP(s) && SCM_CAR(s) == txnid_smob_tag)



/* The scriptable txn API part.
 */

static SCM gls_txn_start(SCM s_tid)
{
	txnid_t tid;
	SCM_ASSERT(txnid_p(s_tid), s_tid, SCM_ARG1, "txn_start");
	tid = txn_start(scm2txnid(s_tid));
	if (tid == -1)
		return SCM_BOOL_F;
	return txnid2scm(tid);
}

static SCM gls_txn_end(SCM s_tid)
{
	SCM_ASSERT(txnid_p(s_tid), s_tid, SCM_ARG1, "txn_end");
	if (txn_end(scm2txnid(s_tid)) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_txn_abort(SCM s_tid)
{
	SCM_ASSERT(txnid_p(s_tid), s_tid, SCM_ARG1, "txn_abort");
	if (txn_abort(scm2txnid(s_tid)) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_txn_undo(SCM s_tid)
{
	txnid_t tid;
	SCM_ASSERT(txnid_p(s_tid), s_tid, SCM_ARG1, "txn_undo");
	tid = txn_undo(scm2txnid(s_tid));
	if (tid == -1)
		return SCM_BOOL_F;
	return txnid2scm(tid);
}

static SCM gls_txn_delete(SCM s_tid)
{
	SCM_ASSERT(txnid_p(s_tid), s_tid, SCM_ARG1, "txn_delete");
	if (txn_delete(scm2txnid(s_tid)) == -1)
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

	SCM_ASSERT(gh_string_p(s_name), s_name, SCM_ARG1, "swapfile_open");
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

	SCM_ASSERT(gh_string_p(s_name), s_name, SCM_ARG1, "swapfile_creat");
	SCM_ASSERT(gh_exact_p(s_size), s_size, SCM_ARG2, "swapfile_creat");
	name = gh_scm2newstr(s_name, &namel);
	res = swapfile_creat(name, gh_scm2long(s_size));
	free(name);
	if (res == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_sw_unlink(SCM s_name)
{
	SCM_ASSERT(gh_exact_p(s_name), s_name, SCM_ARG1, "sw_unlink");
	if (sw_unlink(gh_scm2long(s_name)) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_sw_opendir()
{
	return swdir2scm(sw_opendir());
}

static SCM gls_sw_readdir(SCM s_d)
{
	SCM_ASSERT(swdir_p(s_d), s_d, SCM_ARG1, "sw_readdir");
	return gh_long2scm((long)sw_readdir(scm2swdir(s_d)));
}

static SCM gls_sw_closedir(SCM s_d)
{
	SCM_ASSERT(swdir_p(s_d), s_d, SCM_ARG1, "sw_closedir");
	sw_closedir(scm2swdir(s_d));
	return SCM_BOOL_T;
}

static SCM gls_sw_open(SCM s_name, SCM s_flags, SCM s_tid)
{
	swfd_t fd;

	SCM_ASSERT(gh_exact_p(s_name), s_name, SCM_ARG1, "sw_open");
	SCM_ASSERT(gh_exact_p(s_flags), s_flags, SCM_ARG2, "sw_open");
	SCM_ASSERT(txnid_p(s_tid), s_tid, SCM_ARG3, "sw_open");
	fd = sw_open(gh_scm2long(s_name), gh_scm2long(s_flags),
		     scm2txnid(s_tid));
	if (fd == -1)
		return SCM_BOOL_F;
	return swfd2scm(fd);
}

static SCM gls_sw_close(SCM s_fd)
{
	SCM_ASSERT(swfd_p(s_fd), s_fd, SCM_ARG1, "sw_close");
	if (sw_close(scm2swfd(s_fd)) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_sw_fstat(SCM s_fd)
{
	struct sw_stat st;

	SCM_ASSERT(swfd_p(s_fd), s_fd, SCM_ARG1, "sw_fstat");
	if (sw_fstat(scm2swfd(s_fd), &st) == -1)
		return SCM_BOOL_F;
	return gh_list(gh_long2scm(st.name), gh_long2scm(st.size),
		       gh_long2scm(st.mode), gh_long2scm(st.offset),
		       gh_long2scm(st.cluster_start),
		       gh_long2scm(st.cluster_end),
		       gh_long2scm(st.cluster_size), SCM_UNDEFINED);
}

static SCM gls_sw_ftruncate(SCM s_fd, SCM s_length)
{
	SCM_ASSERT(swfd_p(s_fd), s_fd, SCM_ARG1, "sw_ftruncate");
	SCM_ASSERT(gh_exact_p(s_length), s_length, SCM_ARG2, "sw_ftruncate");
	if (sw_ftruncate(scm2swfd(s_fd), gh_scm2long(s_length)) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_sw_lseek(SCM s_fd, SCM s_offset, SCM s_whence)
{
	long res;
	SCM_ASSERT(swfd_p(s_fd), s_fd, SCM_ARG1, "sw_lseek");
	SCM_ASSERT(gh_exact_p(s_offset), s_offset, SCM_ARG2, "sw_lseek");
	SCM_ASSERT(gh_exact_p(s_whence), s_whence, SCM_ARG3, "sw_lseek");
	res = sw_lseek(scm2swfd(s_fd), gh_scm2long(s_offset),
		       gh_scm2long(s_whence));
	if (res == -1)
		return SCM_BOOL_F;
	return gh_long2scm(res);
}

static SCM gls_sw_sendfile(SCM s_out_fd, SCM s_in_fd, SCM s_count, SCM s_mode)
{
	SCM_ASSERT(swfd_p(s_out_fd), s_out_fd, SCM_ARG1, "sw_sendfile");
	SCM_ASSERT(swfd_p(s_in_fd), s_in_fd, SCM_ARG2, "sw_sendfile");
	SCM_ASSERT(gh_exact_p(s_count), s_count, SCM_ARG3, "sw_sendfile");
	SCM_ASSERT(gh_exact_p(s_mode), s_mode, SCM_ARG4, "sw_sendfile");
	if (sw_sendfile(scm2swfd(s_out_fd), scm2swfd(s_in_fd),
			gh_scm2long(s_count), gh_scm2long(s_mode)) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_sw_read_floatvec(SCM s_fd, SCM s_length)
{
	long length;
	float *m;
	SCM s_vec;

	SCM_ASSERT(swfd_p(s_fd), s_fd, SCM_ARG1, "sw_read_floatvec");
	SCM_ASSERT(gh_exact_p(s_length), s_length,
		   SCM_ARG2, "sw_read_floatvec");
	length = gh_scm2long(s_length);
	m = (float *)malloc(length*sizeof(float));
	if (sw_read(scm2swfd(s_fd), m,
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

	SCM_ASSERT(swfd_p(s_fd), s_fd, SCM_ARG1, "sw_read_string");
	SCM_ASSERT(gh_exact_p(s_length), s_length, SCM_ARG2, "sw_read_string");
	length = gh_scm2long(s_length);
	m = (char *)malloc(length+1);
	if ((res = sw_read(scm2swfd(s_fd), m, length)) != length) {
		free(m);
		return SCM_BOOL_F;
	}
	m[length] = '\0';
	return gh_str02scm(m);
}

static SCM gls_sw_write(SCM s_fd, SCM s_buf)
{
	long res;

	SCM_ASSERT(swfd_p(s_fd), s_fd, SCM_ARG1, "sw_write");
	SCM_ASSERT(gh_vector_p(s_buf) || gh_string_p(s_buf),
		   s_buf, SCM_ARG2, "sw_write");
	if (gh_vector_p(s_buf)) {
		float *fvec;
		long length;
		length = gh_vector_length(s_buf);
		fvec = (float *)malloc(length*sizeof(float));
		gh_scm2floats(s_buf, fvec); /* FIXME!? */
		res = sw_write(scm2swfd(s_fd), fvec, length*sizeof(float));
		free(fvec);
		if (res != -1)
			res /= sizeof(float);
	} else if (gh_string_p(s_buf)) {
		char *str;
		int strlen;
		str = gh_scm2newstr(s_buf, &strlen);
		res = sw_write(scm2swfd(s_fd), str, strlen);
		free(str);
	}
	if (res == -1)
		return SCM_BOOL_F;
	return gh_long2scm(res);
}

int glscript_init_swapfile()
{
	/* Register txnid, swdir and swfd SMOBs to guile. */
	txnid_smob_tag = scm_make_smob_type("txnid",
					    sizeof(struct long_smob));
	scm_set_smob_print(txnid_smob_tag, print_long);
	scm_set_smob_equalp(txnid_smob_tag, equalp_long);
	swdir_smob_tag = scm_make_smob_type("swdir",
					    sizeof(struct pointer_smob));
	scm_set_smob_print(swdir_smob_tag, print_pointer);
	scm_set_smob_equalp(swdir_smob_tag, equalp_pointer);
	swfd_smob_tag = scm_make_smob_type("swfd",
					   sizeof(struct long_smob));
	scm_set_smob_print(swfd_smob_tag, print_long);
	scm_set_smob_equalp(swfd_smob_tag, equalp_long);


	/* Transaction subsystem procedures. */
	gh_new_procedure1_0("txn_start", gls_txn_start);
	gh_new_procedure1_0("txn_end", gls_txn_end);
	gh_new_procedure1_0("txn_abort", gls_txn_abort);
	gh_new_procedure1_0("txn_undo", gls_txn_undo);
	gh_new_procedure1_0("txn_delete", gls_txn_delete);
	gh_define("TXN_NONE", txnid2scm(TXN_NONE));

	/* Swapfile subsystem procedures. */
	gh_new_procedure1_0("swapfile_open", gls_swapfile_open);
	gh_new_procedure0_0("swapfile_close", gls_swapfile_close);
	gh_new_procedure2_0("swapfile_creat", gls_swapfile_creat);

	gh_new_procedure1_0("sw_unlink", gls_sw_unlink);

	gh_new_procedure0_0("sw_opendir", gls_sw_opendir);
	gh_new_procedure1_0("sw_readdir", gls_sw_readdir);
	gh_new_procedure1_0("sw_closedir", gls_sw_closedir);

	gh_new_procedure3_0("sw_open", gls_sw_open);
	gh_new_procedure1_0("sw_close", gls_sw_close);
	gh_new_procedure1_0("sw_fstat", gls_sw_fstat);
	gh_new_procedure2_0("sw_ftruncate", gls_sw_ftruncate);
	gh_new_procedure3_0("sw_lseek", gls_sw_lseek);
	gh_new_procedure4_0("sw_sendfile", gls_sw_sendfile);

	gh_new_procedure2_0("sw_read_floatvec", gls_sw_read_floatvec);
	gh_new_procedure2_0("sw_read_string", gls_sw_read_string);
	gh_new_procedure2_0("sw_write", gls_sw_write);

	gh_define("O_CREAT", gh_long2scm(O_CREAT));
	gh_define("O_EXCL", gh_long2scm(O_EXCL));
	gh_define("O_TRUNC", gh_long2scm(O_TRUNC));
	gh_define("O_RDWR", gh_long2scm(O_RDWR));
	gh_define("O_RDONLY", gh_long2scm(O_RDONLY));
	gh_define("O_WRONLY", gh_long2scm(O_WRONLY));

	gh_define("SWSENDFILE_INSERT", gh_long2scm(SWSENDFILE_INSERT));
	gh_define("SWSENDFILE_CUT", gh_long2scm(SWSENDFILE_CUT));
	gh_define("SW_NOFILE", swfd2scm(SW_NOFILE));

	gh_define("SEEK_SET", gh_long2scm(SEEK_SET));
	gh_define("SEEK_CUR", gh_long2scm(SEEK_CUR));
	gh_define("SEEK_END", gh_long2scm(SEEK_END));

	return 0;
}
