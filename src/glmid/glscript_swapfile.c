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


/* SMOBs for SWDIR, and swfd.
 */

long swdir_smob_tag;
#define scm2swdir(s) (SWDIR *)scm2pointer(s, swdir_smob_tag)
#define swdir2scm(p) pointer2scm((void *)p, swdir_smob_tag)
#define scminvalidateswdir(s) scminvalidatepointer(s, swdir_smob_tag)
#define swdir_p(s) (SCM_NIMP(s) && SCM_CAR(s) == swdir_smob_tag)

long swfd_smob_tag;
#define scm2swfd(s) (swfd_t)scm2long(s, swfd_smob_tag)
#define swfd2scm(p) long2scm(p, swfd_smob_tag)
#define swfd_p(s) (SCM_NIMP(s) && SCM_CAR(s) == swfd_smob_tag)



/* The scriptable swapfile API part.
 * No gls_sw_mmap, gls_sw_munmap.
 */

static SCM gls_swapfile_open(SCM s_name)
{
	char *name;
	int namel, res;

	SCM_ASSERT(gh_string_p(s_name), s_name, SCM_ARG1, "swapfile-open");
	name = gh_scm2newstr(s_name, &namel);
	res = swapfile_open(name, 0);
	free(name);
	if (res == -1)
		GLAME_THROW_ERRNO();
	return SCM_UNSPECIFIED;
}

static SCM gls_swapfile_close()
{
	swapfile_close();
	return SCM_UNSPECIFIED;
}

static SCM gls_swapfile_creat(SCM s_name, SCM s_size)
{
	char *name;
	int namel, res;

	SCM_ASSERT(gh_string_p(s_name), s_name, SCM_ARG1, "swapfile-creat");
	SCM_ASSERT(gh_exact_p(s_size), s_size, SCM_ARG2, "swapfile-creat");
	name = gh_scm2newstr(s_name, &namel);
	res = swapfile_creat(name, gh_scm2long(s_size));
	free(name);
	if (res == -1)
		GLAME_THROW_ERRNO();
	return SCM_UNSPECIFIED;
}

static SCM gls_sw_unlink(SCM s_name)
{
	SCM_ASSERT(gh_exact_p(s_name), s_name, SCM_ARG1, "sw-unlink");
	if (sw_unlink(gh_scm2long(s_name)) == -1)
		GLAME_THROW_ERRNO();
	return SCM_UNSPECIFIED;
}

static SCM gls_sw_opendir()
{
	return swdir2scm(sw_opendir());
}

static SCM gls_sw_readdir(SCM s_d)
{
	SCM_ASSERT(swdir_p(s_d), s_d, SCM_ARG1, "sw-readdir");
	return gh_long2scm((long)sw_readdir(scm2swdir(s_d)));
}

static SCM gls_sw_closedir(SCM s_d)
{
	SCM_ASSERT(swdir_p(s_d), s_d, SCM_ARG1, "sw-closedir");
	if (sw_closedir(scm2swdir(s_d)) == -1)
		GLAME_THROW_ERRNO();
	return SCM_UNSPECIFIED;
}

static SCM gls_sw_open(SCM s_name, SCM s_flags)
{
	swfd_t fd;

	SCM_ASSERT(gh_exact_p(s_name), s_name, SCM_ARG1, "sw-open");
	SCM_ASSERT(gh_exact_p(s_flags), s_flags, SCM_ARG2, "sw-open");
	fd = sw_open(gh_scm2long(s_name), gh_scm2long(s_flags));
	if (fd == -1)
		GLAME_THROW_ERRNO();
	return swfd2scm(fd);
}

static SCM gls_sw_close(SCM s_fd)
{
	SCM_ASSERT(swfd_p(s_fd), s_fd, SCM_ARG1, "sw-close");
	if (sw_close(scm2swfd(s_fd)) == -1)
		GLAME_THROW_ERRNO();
	return SCM_UNSPECIFIED;
}

static SCM gls_sw_fstat(SCM s_fd)
{
	struct sw_stat st;

	SCM_ASSERT(swfd_p(s_fd), s_fd, SCM_ARG1, "sw-fstat");
	if (sw_fstat(scm2swfd(s_fd), &st) == -1)
		GLAME_THROW_ERRNO();
	return gh_list(gh_long2scm(st.name), gh_long2scm(st.size),
		       gh_long2scm(st.mode), gh_long2scm(st.offset),
		       gh_long2scm(st.cluster_start),
		       gh_long2scm(st.cluster_end),
		       gh_long2scm(st.cluster_size), SCM_UNDEFINED);
}

static SCM gls_sw_ftruncate(SCM s_fd, SCM s_length)
{
	SCM_ASSERT(swfd_p(s_fd), s_fd, SCM_ARG1, "sw-ftruncate");
	SCM_ASSERT(gh_exact_p(s_length), s_length, SCM_ARG2, "sw-ftruncate");
	if (sw_ftruncate(scm2swfd(s_fd), gh_scm2long(s_length)) == -1)
		GLAME_THROW_ERRNO();
	return SCM_UNSPECIFIED;
}

static SCM gls_sw_lseek(SCM s_fd, SCM s_offset, SCM s_whence)
{
	long res;
	SCM_ASSERT(swfd_p(s_fd), s_fd, SCM_ARG1, "sw-lseek");
	SCM_ASSERT(gh_exact_p(s_offset), s_offset, SCM_ARG2, "sw-lseek");
	SCM_ASSERT(gh_exact_p(s_whence), s_whence, SCM_ARG3, "sw-lseek");
	res = sw_lseek(scm2swfd(s_fd), gh_scm2long(s_offset),
		       gh_scm2long(s_whence));
	if (res == -1)
		GLAME_THROW_ERRNO();
	return gh_long2scm(res);
}

static SCM gls_sw_sendfile(SCM s_out_fd, SCM s_in_fd, SCM s_count, SCM s_mode)
{
	int mode = 0;
	SCM_ASSERT(swfd_p(s_out_fd), s_out_fd, SCM_ARG1, "sw-sendfile");
	SCM_ASSERT(swfd_p(s_in_fd), s_in_fd, SCM_ARG2, "sw-sendfile");
	SCM_ASSERT(gh_exact_p(s_count), s_count, SCM_ARG3, "sw-sendfile");
	SCM_ASSERT(SCM_UNBNDP(s_mode) || gh_exact_p(s_mode),
		   s_mode, SCM_ARG4, "sw-sendfile");
	if (!SCM_UNBNDP(s_mode))
		mode = gh_scm2long(s_mode);
	if (sw_sendfile(scm2swfd(s_out_fd), scm2swfd(s_in_fd),
			gh_scm2long(s_count), mode) == -1)
		GLAME_THROW_ERRNO();
	return SCM_UNSPECIFIED;
}

static SCM gls_sw_read_floatvec(SCM s_fd, SCM s_length)
{
	long length;
	float *m;
	SCM s_vec;

	SCM_ASSERT(swfd_p(s_fd), s_fd, SCM_ARG1, "sw-read-floatvec");
	SCM_ASSERT(gh_exact_p(s_length), s_length,
		   SCM_ARG2, "sw-read-floatvec");
	length = gh_scm2long(s_length);
	m = (float *)malloc(length*sizeof(float));
	if (sw_read(scm2swfd(s_fd), m,
		    length*sizeof(float)) != length*sizeof(float)) {
		free(m);
		GLAME_THROW_ERRNO();
	}
	s_vec = gh_floats2fvect(m, length);
	free(m); /* FIXME!?? */
	return s_vec;
}

static SCM gls_sw_read_string(SCM s_fd, SCM s_length)
{
	long length, res;
	char *m;

	SCM_ASSERT(swfd_p(s_fd), s_fd, SCM_ARG1, "sw-read-string");
	SCM_ASSERT(gh_exact_p(s_length), s_length, SCM_ARG2, "sw-read-string");
	length = gh_scm2long(s_length);
	m = (char *)malloc(length+1);
	if ((res = sw_read(scm2swfd(s_fd), m, length)) != length) {
		free(m);
		GLAME_THROW_ERRNO();
	}
	m[length] = '\0';
	return gh_str02scm(m);
}

static SCM gls_sw_write(SCM s_fd, SCM s_buf)
{
	long res;

	SCM_ASSERT(swfd_p(s_fd), s_fd, SCM_ARG1, "sw-write");
	SCM_ASSERT(gh_vector_p(s_buf) || gh_string_p(s_buf),
		   s_buf, SCM_ARG2, "sw-write");
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
		GLAME_THROW_ERRNO();
	return gh_long2scm(res);
}

static SCM gls_is_swfd(SCM s_fd)
{
	if (swfd_p(s_fd))
		return SCM_BOOL_T;
	return SCM_BOOL_F;
}

static SCM gls_is_swdir(SCM s_dir)
{
	if (swdir_p(s_dir))
		return SCM_BOOL_T;
	return SCM_BOOL_F;
}

int glscript_init_swapfile()
{
	/* Register swdir and swfd SMOBs to guile. */
	swdir_smob_tag = scm_make_smob_type("swdir",
					    sizeof(struct pointer_smob));
	scm_set_smob_print(swdir_smob_tag, print_pointer);
	scm_set_smob_equalp(swdir_smob_tag, equalp_pointer);
	swfd_smob_tag = scm_make_smob_type("swfd",
					   sizeof(struct long_smob));
	scm_set_smob_print(swfd_smob_tag, print_long);
	scm_set_smob_equalp(swfd_smob_tag, equalp_long);


	/* Swapfile subsystem procedures. */
	gh_new_procedure1_0("swapfile-open", gls_swapfile_open);
	gh_new_procedure0_0("swapfile-close", gls_swapfile_close);
	gh_new_procedure2_0("swapfile-creat", gls_swapfile_creat);

	gh_new_procedure1_0("sw-unlink", gls_sw_unlink);

	gh_new_procedure0_0("sw-opendir", gls_sw_opendir);
	gh_new_procedure1_0("sw-readdir", gls_sw_readdir);
	gh_new_procedure1_0("sw-closedir", gls_sw_closedir);

	gh_new_procedure2_0("sw-open", gls_sw_open);
	gh_new_procedure1_0("sw-close", gls_sw_close);
	gh_new_procedure1_0("sw-fstat", gls_sw_fstat);
	gh_new_procedure2_0("sw-ftruncate", gls_sw_ftruncate);
	gh_new_procedure3_0("sw-lseek", gls_sw_lseek);
	gh_new_procedure("sw-sendfile", (SCM (*)())gls_sw_sendfile,
			 3, 1, 0);

	gh_new_procedure2_0("sw-read-floatvec", gls_sw_read_floatvec);
	gh_new_procedure2_0("sw-read-string", gls_sw_read_string);
	gh_new_procedure2_0("sw-write", gls_sw_write);

	gh_new_procedure1_0("swfd?", gls_is_swfd);
	gh_new_procedure1_0("swdir?", gls_is_swdir);

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

	/* compatibility */
	gh_new_procedure1_0("swapfile_open", gls_swapfile_open);
	gh_new_procedure0_0("swapfile_close", gls_swapfile_close);
	gh_new_procedure2_0("swapfile_creat", gls_swapfile_creat);
	gh_new_procedure1_0("sw_unlink", gls_sw_unlink);
	gh_new_procedure0_0("sw_opendir", gls_sw_opendir);
	gh_new_procedure1_0("sw_readdir", gls_sw_readdir);
	gh_new_procedure1_0("sw_closedir", gls_sw_closedir);
	gh_new_procedure2_0("sw_open", gls_sw_open);
	gh_new_procedure1_0("sw_close", gls_sw_close);
	gh_new_procedure1_0("sw_fstat", gls_sw_fstat);
	gh_new_procedure2_0("sw_ftruncate", gls_sw_ftruncate);
	gh_new_procedure3_0("sw_lseek", gls_sw_lseek);
	gh_new_procedure("sw_sendfile", (SCM (*)())gls_sw_sendfile,
			 3, 1, 0);
	gh_new_procedure2_0("sw_read_floatvec", gls_sw_read_floatvec);
	gh_new_procedure2_0("sw_read_string", gls_sw_read_string);
	gh_new_procedure2_0("sw_write", gls_sw_write);


	return 0;
}
