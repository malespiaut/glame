/*  Guile 1.4/1.6 compatibility layer -- part of GLAME
    Copyright (C) 2002  Clinton Ebadi

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

/* NEW_GUILE is set to 1 if we are using Guile > 1.5, 0 for Guile 1.4
   to make it possible to make localized compatibility that doesn't
   have to be in this header because the feature isn't use much.
*/

/* Big issues that need to be dealt with: 
 * - Guile 1.4, 1.6, and 1.7/8 compatibility? 
 *   maybe some other time...GLAME will work with 1.4 and
 *   1.6 and that is what matters right now
 * guile14, 16, and 17.h
 */
#ifndef GLAME_GUILE_COMPAT_H
#define GLAME_GUILE_COMPAT_H

#include <libguile.h>

#ifdef SCM_MAJOR_VERSION
/* this works because Guile 1.4 doesn't define SCM_MAJOR_VERSION */

#define NEW_GUILE 1

/* Glame wrapper functions: */
#if (SCM_MINOR_VERSION < 7)
/* use GLAME_NEWCELL instead of SCM_NEWCELL */
#define GLAME_NEWCELL(z) SCM_NEWCELL(z)
#endif
#define glame_scm2newstr(obj, lenp) gh_scm2newstr (obj, lenp)
#define glame_scm2long(obj) scm_num2long (obj, SCM_ARG1, "somewhere")
#define glame_scm2double(obj) scm_num2dbl (obj, "somewhere")
#define scm_str2string(str) gh_str02scm (str)

#if (SCM_MINOR_VERSION == 7)
#define GLAME_NEWCELL(z) z = scm_cell (SCM_UNPACK(z), 0)
#endif

#else /* Guile 1.4.x */

#include <guile/gh.h>

#define NEW_GUILE 0
#define SCM_MAJOR_VERSION 1
#define SCM_MINOR_VERSION 4
#define SCM_MICRO_VERSION 0

/* Glame wrapper functions: */
#define GLAME_NEWCELL(z) SCM_NEWCELL(z)
#define glame_scm2newstr(obj, lenp) gh_scm2newstr (obj, lenp)
#define glame_scm2long(obj) gh_scm2long (obj)
#define glame_scm2double(obj) gh_scm2double (obj)

/* 1.6 compat stuff: (make guile 1.4 use the 1.6 api) */
/* If a function cannot be easily defined in terms of a 1.4
   function, then write a glame wrapper or reimplement the
   version from Guile 1.6 using internal 1.4 stuff...
   if stuff in here gets too large, maybe a glame_guile_compat.c
   should be written 
*/

#define scm_c_define_gsubr(func_name, req, opt, rest, func) gh_new_procedure(func_name, func, req, opt, rest)
#define scm_c_export(sym, ...) /* nothing */
#define scm_c_define(sym, val) gh_define (sym, val)
#define scm_long2num(num) gh_long2scm (num)
#define scm_double2num(num) gh_double2scm (num)
#define scm_cons(a, b) gh_cons (a, b)
#define scm_makfrom0str(str) gh_str02scm (str)
#define scm_str2string(str) gh_str02scm (str)

/* types */
#define scm_t_port scm_port
#endif /* defined SCM_MAJOR_VERSION */
#endif /* GLAME_GUILE_COMPAT_H */
