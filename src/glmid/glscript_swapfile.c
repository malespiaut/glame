/*
 * glscript_swapfile.c
 *
 * Copyright (C) 2000, 2001, 2002, 2003, 2004 Richard Guenther
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

long swfd_smob_tag = 0;
struct swfd_smob {
	swfd_t swfd;
};
#define SCM2SWFDSMOB(s) ((struct swfd_smob *)SCM_SMOB_DATA(s))
SCM swfd2scm(swfd_t swfd);
swfd_t scm2swfd(SCM swfd_smob);

static size_t free_swfd(SCM swfd_smob)
{
	struct swfd_smob *swfd = SCM2SWFDSMOB(swfd_smob);

	/* Delete the swfd, if it is neither plugin, nor part
	 * of a network and not running. */
	if (swfd->swfd != -1) {
		DPRINTF("GCing swfd %li\n", (long)(swfd->swfd));
		sw_close(swfd->swfd);
		swfd->swfd = -1;
	}

	return sizeof(struct swfd_smob);
}

static int print_swfd(SCM swfd_smob, SCM port, scm_print_state *pstate)
{
	struct swfd_smob *swfd = SCM2SWFDSMOB(swfd_smob);
	char buf[256];

	snprintf(buf, 255, "#<swfd %li>", (long)(swfd->swfd));
	scm_puts(buf, port);

	return 1;
}

static SCM equalp_swfd(SCM swfd_smob1, SCM swfd_smob2)
{
	struct swfd_smob *swfd1 = SCM2SWFDSMOB(swfd_smob1);
	struct swfd_smob *swfd2 = SCM2SWFDSMOB(swfd_smob2);

	if (swfd1->swfd == swfd2->swfd)
		return SCM_BOOL_T;
	return SCM_BOOL_F;
}

SCM swfd2scm(swfd_t swfd)
{
	struct swfd_smob *smob;
	SCM swfd_smob;

	if (!swfd)
		GLAME_THROW();

	smob = (struct swfd_smob *)malloc(sizeof(struct swfd_smob));
	smob->swfd = swfd;

	SCM_NEWSMOB(swfd_smob, swfd_smob_tag, smob);

	return swfd_smob;
}

swfd_t scm2swfd(SCM swfd_smob)
{
	SCM_ASSERT(swfd_p(swfd_smob),
		   swfd_smob, SCM_ARG1, "scm2swfd");
	return SCM2SWFDSMOB(swfd_smob)->swfd;
}

void scminvalidateswfd(SCM swfd_smob)
{
	struct swfd_smob *swfd = SCM2SWFDSMOB(swfd_smob);

	SCM_ASSERT(swfd_p(swfd_smob),
		   swfd_smob, SCM_ARG1, "scminvalidateswfd");

	swfd->swfd = -1;
}




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
	return scm_long2num((long)sw_readdir(scm2swdir(s_d)));
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
	swfd_t fd;
	SCM_ASSERT(swfd_p(s_fd), s_fd, SCM_ARG1, "sw-close");
	fd = scm2swfd(s_fd);
	scminvalidateswfd(s_fd);
	if (sw_close(fd) == -1)
		GLAME_THROW_ERRNO();
	return SCM_UNSPECIFIED;
}

static SCM gls_sw_fstat(SCM s_fd)
{
	struct sw_stat st;

	SCM_ASSERT(swfd_p(s_fd), s_fd, SCM_ARG1, "sw-fstat");
	if (sw_fstat(scm2swfd(s_fd), &st) == -1)
		GLAME_THROW_ERRNO();
	return gh_list(scm_long2num(st.name), scm_long2num(st.size),
		       scm_long2num(st.mode), scm_long2num(st.offset),
		       scm_long2num(st.cluster_start),
		       scm_long2num(st.cluster_end),
		       scm_long2num(st.cluster_size), SCM_UNDEFINED);
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
	return scm_long2num(res);
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
	return scm_makfrom0str(m);
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
	} else /* if (gh_string_p(s_buf)) */ {
		char *str;
		int strlen;
		str = gh_scm2newstr(s_buf, &strlen);
		res = sw_write(scm2swfd(s_fd), str, strlen);
		free(str);
	}
	if (res == -1)
		GLAME_THROW_ERRNO();
	return scm_long2num(res);
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
					   sizeof(struct swfd_smob));
	scm_set_smob_free(swfd_smob_tag, free_swfd);
	scm_set_smob_print(swfd_smob_tag, print_swfd);
	scm_set_smob_equalp(swfd_smob_tag, equalp_swfd);


	/* Swapfile subsystem procedures. */
	glame_reg_export ("swapfile-open", 1, 0, 0, gls_swapfile_open);
	glame_reg_export ("swapfile-close", 0, 0, 0, gls_swapfile_close);
	glame_reg_export ("swapfile-creat", 2, 0, 0, gls_swapfile_creat);

	glame_reg_export ("sw-unlink", 1, 0, 0, gls_sw_unlink);

	glame_reg_export ("sw-opendir", 0, 0, 0, gls_sw_opendir);
	glame_reg_export ("sw-readdir", 1, 0, 0, gls_sw_readdir);
	glame_reg_export ("sw-closedir", 1, 0, 0, gls_sw_closedir);

	glame_reg_export ("sw-open", 2, 0, 0, gls_sw_open);
	glame_reg_export ("sw-close", 1, 0, 0, gls_sw_close);
	glame_reg_export ("sw-fstat", 1, 0, 0, gls_sw_fstat);
	glame_reg_export ("sw-ftruncate", 2, 0, 0, gls_sw_ftruncate);
	glame_reg_export ("sw-lseek", 3, 0, 0, gls_sw_lseek);
	glame_reg_export ("sw-sendfile", 3, 1, 0,gls_sw_sendfile);

	glame_reg_export ("sw-read-floatvec", 2, 0, 0, gls_sw_read_floatvec);
	glame_reg_export ("sw-read-string", 2, 0, 0, gls_sw_read_string);
	glame_reg_export ("sw-write", 2, 0, 0, gls_sw_write);

	glame_reg_export ("swfd?", 1, 0, 0, gls_is_swfd);
	glame_reg_export ("swdir?", 1, 0, 0, gls_is_swdir);

	glame_def_export ("O_CREAT", scm_long2num(O_CREAT));
	glame_def_export ("O_EXCL", scm_long2num(O_EXCL));
	glame_def_export ("O_TRUNC", scm_long2num(O_TRUNC));
	glame_def_export ("O_RDWR", scm_long2num(O_RDWR));
	glame_def_export ("O_RDONLY", scm_long2num(O_RDONLY));
	glame_def_export ("O_WRONLY", scm_long2num(O_WRONLY));

	glame_def_export ("SWSENDFILE_INSERT", 
			  scm_long2num(SWSENDFILE_INSERT));
	glame_def_export ("SWSENDFILE_CUT", scm_long2num(SWSENDFILE_CUT));
	glame_def_export ("SW_NOFILE", swfd2scm(SW_NOFILE));

	glame_def_export ("SEEK_SET", scm_long2num(SEEK_SET));
	glame_def_export ("SEEK_CUR", scm_long2num(SEEK_CUR));
	glame_def_export ("SEEK_END", scm_long2num(SEEK_END));

	return 0;
}
