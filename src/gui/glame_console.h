#ifndef _GLAME_CONSOLE_H
#define _GLAME_CONSOLE_H

/*
 * glame_console.h
 *
 * $Id: glame_console.h,v 1.3 2003/05/26 18:21:51 richi Exp $
 *
 * Copyright (C) 2001-2003 Richard Guenther
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

#ifdef __cplusplus
extern "C" {
#endif

/* Init the console and create the widget, but do not show it.
 * Returns 0 on success, -1 on error. */
int glame_console_init();

/* Shows the GLAME console. */
void glame_console_hide();

/* Hides the GLAME console. */
void glame_console_show();


/* Outputs on the GLAME console just like printf does. Returns the
 * number of characters written. */
int glame_console_printf(const char *format, ...);

#ifdef __cplusplus
}
#endif

#endif
