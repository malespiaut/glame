/*
 * glscript.c
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
#include <filter.h>
#include <glplugin.h>


extern int glscript_init_swapfile();
extern int glscript_init_filter();


/* SMOB for generic C pointer.
 */

static long pointer_smob_tag = 0;
struct pointer_smob {
	void *pointer;
};
#define SCM2POINTERSMOB(s) ((struct pointer_smob *)SCM_CDR(s))

static int print_pointer(SCM pointer_smob, SCM port, scm_print_state *pstate)
{
	struct pointer_smob *pointer = SCM2POINTERSMOB(pointer_smob);
	char buf[256];

	snprintf(buf, 255, "#<pointer %p>", pointer->pointer);
	scm_puts(buf, port);

	return 1;
}

static SCM equalp_pointer(SCM pointer_smob1, SCM pointer_smob2)
{
	struct pointer_smob *pointer1 = SCM2POINTERSMOB(pointer_smob1);
	struct pointer_smob *pointer2 = SCM2POINTERSMOB(pointer_smob2);

	if (pointer1->pointer == pointer2->pointer)
		return SCM_BOOL_T;
	return SCM_BOOL_F;
}


SCM pointer2scm(void *pointer)
{
	struct pointer_smob *smob;
	SCM pointer_smob;

	if (!pointer)
		return SCM_BOOL_F;

	smob = (struct pointer_smob *)malloc(sizeof(struct pointer_smob));
	smob->pointer = pointer;

	SCM_NEWSMOB(pointer_smob, pointer_smob_tag, smob);

	return pointer_smob;
}

void *scm2pointer(SCM pointer_smob)
{
	struct pointer_smob *pointer = SCM2POINTERSMOB(pointer_smob);

	SCM_ASSERT((SCM_NIMP(pointer_smob)
		    && SCM_CAR(pointer_smob) == pointer_smob_tag),
		   pointer_smob, SCM_ARG1, "scm2pointer");

	return pointer->pointer;
}

void scminvalidatepointer(SCM pointer_smob)
{
	struct pointer_smob *pointer = SCM2POINTERSMOB(pointer_smob);

	SCM_ASSERT((SCM_NIMP(pointer_smob)
		    && SCM_CAR(pointer_smob) == pointer_smob_tag),
		   pointer_smob, SCM_ARG1, "scm2pointer");

	pointer->pointer = NULL;
}

static SCM pointerp(SCM pointer_smob)
{
	if (!SCM_NIMP(pointer_smob)
	    || SCM_CAR(pointer_smob) != pointer_smob_tag)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}



int glscript_init()
{
	/* Tell scheme about installation directory of GLAME.
	 */
	gh_eval_str("(define glamedir \"" PKGDATADIR "\")");

	/* Register the pointer SMOB to guile. */
	pointer_smob_tag = scm_make_smob_type("pointer", sizeof(struct pointer_smob));
	scm_set_smob_print(pointer_smob_tag, print_pointer);
	scm_set_smob_equalp(pointer_smob_tag, equalp_pointer);
	gh_new_procedure("pointer?", (SCM (*)())pointerp, 1, 0, 0);

	/* Register scheme procedures for the subsystems.
	 */
	if (glscript_init_swapfile() == -1
	    || glscript_init_filter() == -1)
		return -1;

	/* Load glame scheme libraries (if existent):
	 * 1. installed glame.scm
	 * 2. glmid/glame.scm
	 * 3. ~/.glame.scm
	 */
	gh_eval_str(
"(map (lambda (file)"
"        (if (file-exists? file)"
"           (begin"
#ifdef DEBUG
"              (display (string-append \"loading \" file)) (newline)"
#endif
"              (load file))))"
"  `(\"" PKGDATADIR "/glame.scm\""
"    \"glmid/glame.scm\""
"    ,(string-append (getenv \"HOME\") \"/.glame.scm\")))");

	return 0;
}
