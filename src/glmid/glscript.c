/*
 * glscript.c
 *
 * Copyright (C) 2000 Richard Guenther
 * Copyright (C) 2002 Clinton Ebadi
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
#include "filter.h"
#include "glplugin.h"
#include "glscript.h"



/* SMOB for generic C pointer.
 */

int print_pointer(SCM pointer_smob, SCM port, scm_print_state *pstate)
{
	struct pointer_smob *pointer = SCM2POINTERSMOB(pointer_smob);
	char buf[256];

	snprintf(buf, 255, "#<pointer %p>", pointer->pointer);
	scm_puts(buf, port);

	return 1;
}

SCM equalp_pointer(SCM pointer_smob1, SCM pointer_smob2)
{
	struct pointer_smob *pointer1 = SCM2POINTERSMOB(pointer_smob1);
	struct pointer_smob *pointer2 = SCM2POINTERSMOB(pointer_smob2);

	if (pointer1->pointer == pointer2->pointer)
		return SCM_BOOL_T;
	return SCM_BOOL_F;
}


SCM pointer2scm(void *pointer, long smob_tag)
{
	struct pointer_smob *smob;
	SCM pointer_smob;

	if (!pointer)
		return SCM_BOOL_F;

	smob = (struct pointer_smob *)malloc(sizeof(struct pointer_smob));
	smob->pointer = pointer;

	SCM_NEWSMOB(pointer_smob, smob_tag, smob);

	return pointer_smob;
}

void *scm2pointer(SCM pointer_smob, long smob_tag)
{
	SCM_ASSERT(SCM_SMOB_PREDICATE(smob_tag, pointer_smob),
		   pointer_smob, SCM_ARG1, "scm2pointer");
	return SCM2POINTERSMOB(pointer_smob)->pointer;
}

void scminvalidatepointer(SCM pointer_smob, long smob_tag)
{
	struct pointer_smob *pointer = SCM2POINTERSMOB(pointer_smob);

	SCM_ASSERT(SCM_SMOB_PREDICATE(smob_tag, pointer_smob),
		   pointer_smob, SCM_ARG1, "scminvalidatepointer");

	pointer->pointer = NULL;
}


/* SMOB for generic C long.
 */

int print_long(SCM long_smob, SCM port, scm_print_state *pstate)
{
	struct long_smob *val = SCM2LONGSMOB(long_smob);
	char buf[256];

	snprintf(buf, 255, "#<long %li>", val->val);
	scm_puts(buf, port);

	return 1;
}

SCM equalp_long(SCM long_smob1, SCM long_smob2)
{
	struct long_smob *long1 = SCM2LONGSMOB(long_smob1);
	struct long_smob *long2 = SCM2LONGSMOB(long_smob2);

	if (long1->val == long2->val)
		return SCM_BOOL_T;
	return SCM_BOOL_F;
}


SCM long2scm(long val, long smob_tag)
{
	struct long_smob *smob;
	SCM long_smob;

	smob = (struct long_smob *)malloc(sizeof(struct long_smob));
	smob->val = val;

	SCM_NEWSMOB(long_smob, smob_tag, smob);

	return long_smob;
}

long scm2long(SCM long_smob, long smob_tag)
{
	SCM_ASSERT(SCM_SMOB_PREDICATE(smob_tag, long_smob),
		   long_smob, SCM_ARG1, "scm2long");
	return SCM2LONGSMOB(long_smob)->val;
}

static void _glscript_init (void* unused)
{
	/* Tell scheme about installation directory of GLAME
	 * and the revision of the scripting language.
	 */
	glame_def_export ("glamedir", scm_makfrom0str(PKGSCRIPTSDIR));
	glame_def_export ("glameversion", scm_long2num(1));

	/* Register scheme procedures for the subsystems.
	 */

	glscript_init_swapfile();
	glscript_init_filter();
	glscript_init_gpsm();
}

#if !NEW_GUILE
static void _gl_init_wrap ()
{
	/* scm_register_module_xxx wants a procedure that takes no args, 
	   scm_c_define_module wants one that takes a void* */
	_glscript_init (NULL);
}
#endif

int glscript_init()
{
#if NEW_GUILE
	SCM glame_user_module;
#endif
#ifdef DEBUG
	scm_eval_string(scm_str2string("(debug-enable 'backtrace)\n"
				       "(debug-enable 'debug)\n"
				       "(read-enable 'positions)\n"));
#endif /* DEBUG */

	/* Redirect output/error to console - redirected again after
	 * gui/glame_console init. */
	scm_set_current_output_port(
		scm_fdes_to_port(dup(1), "w", scm_makfrom0str("stdout")));
	scm_set_current_error_port(
		scm_fdes_to_port(dup(2), "w", scm_makfrom0str ("stderr")));

#if NEW_GUILE
	/* define the glame module */
	scm_c_define_module ("glame", _glscript_init, NULL);

 	/* Switch to a more useful module and use the glame module. */
	glame_user_module = scm_c_define_module ("guile-user", NULL, NULL);
	scm_set_current_module (glame_user_module);
	scm_c_use_module ("glame");
	scm_c_use_module ("guile");
	scm_c_use_module ("ice-9 session");

#else
	/* define the glame module */
	scm_register_module_xxx ("glame", _gl_init_wrap);

	/* Register all GLAME specific stuff inside the "glame"
	 * module. Do this with lazy guile 1.3.4/1.4 stuff - oh well. */
	scm_register_module_xxx("glame", _glscript_init);

	/* Switch to a more useful module and use the glame module. */
	gh_eval_str(
"(define-module (guile-user)"
"  :use-module (glame)"
"  :use-module (guile)"
"  :use-module (ice-9 session))");
#endif



	/* Load glame scheme libraries (if existent):
	 * 1. installed glame.scm
	 * 2. glmid/glame.scm
	 * 3. ~/.glame.scm
	 */
	glame_gh_safe_eval_str(
"(map (lambda (file)"
"        (if (file-exists? file)"
"           (begin"
#ifdef DEBUG
"              (display (string-append \"loading \" file)) (newline)"
#endif
"              (load file))))"
"  `(\"" PKGSCRIPTSDIR "/glame.scm\""
"    \"glmid/glame.scm\""
"    ,(string-append (getenv \"HOME\") \"/.glame.scm\")))");

	return 0;
}
