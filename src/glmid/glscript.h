#ifndef _GLSCRIPT_H
#define _GLSCRIPT_H

/*
 * glscript.h
 *
 * Copyright (C) 2000, 2001, 2002, 2004 Richard Guenther, Clinton Ebadi
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
#include <glame_guile_compat.h>
#include "filter.h"
#include "gpsm.h"
#include "swapfile.h"


/* Initializes the guile scripting subsystem. Returns 0 on success,
 * -1 on error. */
int glscript_init();


/* Private init functions - for internal use only. */
int glscript_init_swapfile();
int glscript_init_filter();
int glscript_init_gpsm();


/* HACK to allow switching between register(0)/instantiate(1) mode */
extern int glscript_load_mode;
extern filter_t *last_loaded_filter_instance;


/* Safe wrappers for gh_eval_file/gh_eval_string. Result is #f if
 * anything went wrong, #unspecified otherwise. */
static inline SCM glame_gh_safe_eval_file(const char *fname)
{
	SCM s_res;
	s_res = gh_eval_file_with_catch((char *)fname, scm_handle_by_message_noexit);
	scm_flush(scm_current_output_port());
	scm_flush(scm_current_error_port());
	return s_res;
}
static inline SCM glame_gh_safe_eval_str(const char *str)
{
	SCM s_res;
	s_res = gh_eval_str_with_catch((char *)str, 
				       scm_handle_by_message_noexit);
	scm_flush(scm_current_output_port());
	scm_flush(scm_current_error_port());
	return s_res;
}


/* Throw a 'glame-error exception with no arguments. This is the
 * generic method to signal errors from scm. */
#define GLAME_THROW() scm_throw(gh_symbol2scm("glame-error"), SCM_EOL)

/* Throw a 'glame-error exception with an object. */
#define GLAME_THROW_OBJ(obj) scm_throw(gh_symbol2scm("glame-error"), scm_cons(obj, SCM_EOL))

/* Throw a 'glame-error exception with strerror(errno). */
#define GLAME_THROW_ERRNO() scm_throw(gh_symbol2scm("glame-error"), scm_cons(scm_makfrom0str(strerror(errno)), SCM_EOL))



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


/* SMOBs for gpsm_item_t, ...
 */

extern long gpsmitem_smob_tag;
#define gpsmitem_p(s) (SCM_SMOB_PREDICATE(gpsmitem_smob_tag, (s)))
SCM gpsmitem2scm(gpsm_item_t *item);
gpsm_item_t *scm2gpsmitem(SCM gpsmitem_smob);
void scminvalidategpsmitem(SCM gpsmitem_smob);

extern long pipe_smob_tag;
#define scm2pipe(s) ((filter_pipe_t *)scm2pointer(s, pipe_smob_tag))
#define pipe2scm(p) pointer2scm(p, pipe_smob_tag)
#define scminvalidatepipe(s) scminvalidatepointer(s, pipe_smob_tag)
#define pipe_p(s) (SCM_SMOB_PREDICATE(pipe_smob_tag, (s)))

extern long port_smob_tag;
#define scm2port(s) ((filter_port_t *)scm2pointer(s, port_smob_tag))
#define port2scm(p) pointer2scm(p, port_smob_tag)
#define scminvalidateport(s) scminvalidatepointer(s, port_smob_tag)
#define port_p(s) (SCM_SMOB_PREDICATE(port_smob_tag, (s)))

extern long param_smob_tag;
#define scm2param(s) ((filter_param_t *)scm2pointer(s, param_smob_tag))
#define param2scm(p) pointer2scm(p, param_smob_tag)
#define scminvalidateparam(s) scminvalidatepointer(s, param_smob_tag)
#define param_p(s) (SCM_SMOB_PREDICATE(param_smob_tag, (s)))

extern long plugin_smob_tag;
#define scm2plugin(s) ((plugin_t *)scm2pointer(s, plugin_smob_tag))
#define plugin2scm(p) pointer2scm(p, plugin_smob_tag)
#define scminvalidateplugin(s) scminvalidatepointer(s, plugin_smob_tag)
#define plugin_p(s) (SCM_SMOB_PREDICATE(plugin_smob_tag, (s)))

extern long filter_smob_tag;
filter_t *scm2filter(SCM filter_smob);
SCM filter2scm(filter_t *filter);
void scminvalidatefilter(SCM filter_smob);
#define filter_p(s) (SCM_SMOB_PREDICATE(filter_smob_tag, (s)))

extern long swdir_smob_tag;
#define scm2swdir(s) (SWDIR *)scm2pointer(s, swdir_smob_tag)
#define swdir2scm(p) pointer2scm((void *)p, swdir_smob_tag)
#define scminvalidateswdir(s) scminvalidatepointer(s, swdir_smob_tag)
#define swdir_p(s) (SCM_SMOB_PREDICATE(swdir_smob_tag, (s)))

extern long swfd_smob_tag;
SCM swfd2scm(swfd_t swfd);
swfd_t scm2swfd(SCM swfd_smob);
void scminvalidateswfd(SCM swfd_smob);
#define swfd_p(s) (SCM_SMOB_PREDICATE(swfd_smob_tag, (s)))

/* glame_reg_export
 * This macro adds func to the current module and exports it
 * to the outside world. This makes the code in other places
 * much more readable (and easier to maintain).
 * ARGUMENTS:
 * func_name: string representing the name Scheme sees
 * req: number of required arguments
 * opt: number of optional arguments
 * rest: 1 if function takes unlimited args, 0 otherwise
 */
#define glame_reg_export(func_name, req, opt, rest, func) \
scm_c_define_gsubr (func_name, req, opt, rest, func); \
scm_c_export (func_name, NULL)

/* glame_def_export
 * This macro adds a new definition to the current module and exports 
 * it to the outside world. This is done for the same reason as
 * glame_reg_export (this replaces gh_define).
 */
#define glame_def_export(name, value) \
scm_c_define (name, value); \
scm_c_export (name, NULL)
#endif
