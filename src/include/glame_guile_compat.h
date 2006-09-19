/*  Guile 1.4/1.6 compatibility layer -- part of GLAME
    Copyright (C) 2002, 2003, 2004  Clinton Ebadi, Richard Guenther

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

/* Glame wrapper functions: */
#define glame_scm2newstr(obj, lenp) gh_scm2newstr (obj, lenp)
#define glame_scm2long(obj) scm_num2long (obj, SCM_ARG1, "somewhere")
#define glame_scm2double(obj) scm_num2dbl (obj, "somewhere")
#define scm_str2string(str) gh_str02scm (str)

#endif /* GLAME_GUILE_COMPAT_H */
