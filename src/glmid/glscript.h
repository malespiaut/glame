#ifndef _GLSCRIPT_H
#define _GLSCRIPT_H

/*
 * glscript.h
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
#include "filter.h"


/* Initializes the guile scripting subsystem. Returns 0 on success,
 * -1 on error. */
int glscript_init();


/* Private init functions - for internal use only. */
int glscript_init_swapfile();
int glscript_init_filter();


/* HACK to allow switching between register(0)/instantiate(1) mode */
extern int glscript_load_mode;
extern filter_t *last_loaded_filter_instance;


/* Safe wrappers for gh_eval_file/gh_eval_string. Result is #f if
 * anything went wrong, #t otherwise. */
static inline SCM glame_gh_safe_handler(void *a, SCM b, SCM c)
{
	return SCM_BOOL_F;
}
static inline SCM glame_gh_safe_eval_file(const char *fname)
{
	return gh_eval_file_with_catch((char *)fname, glame_gh_safe_handler);
}
static inline SCM glame_gh_safe_eval_str(const char *str)
{
	return gh_eval_str_with_catch((char *)str, glame_gh_safe_handler);
}


/* SMOB for generic C pointer - for internal use only.
 */

struct pointer_smob {
	void *pointer;
};
#define SCM2POINTERSMOB(s) ((struct pointer_smob *)SCM_SMOB_DATA(s))

int print_pointer(SCM pointer_smob, SCM port, scm_print_state *pstate);
SCM equalp_pointer(SCM pointer_smob1, SCM pointer_smob2);
SCM pointer2scm(void *pointer, long smob_tag);
void *scm2pointer(SCM pointer_smob, long smob_tag);
void scminvalidatepointer(SCM pointer_smob, long smob_tag);


/* SMOB for generic C long - for internal use only.
 */

struct long_smob {
	long val;
};
#define SCM2LONGSMOB(s) ((struct long_smob *)SCM_SMOB_DATA(s))

int print_long(SCM long_smob, SCM port, scm_print_state *pstate);
SCM equalp_long(SCM long_smob1, SCM long_smob2);
SCM long2scm(long val, long smob_tag);
long scm2long(SCM long_smob, long smob_tag);


#endif
