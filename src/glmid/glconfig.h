#ifndef _GLAME_CONFIG_H
#define _GLAME_CONFIG_H

/*
 * glconfig.h
 *
 * $Id: glconfig.h,v 1.4 2004/10/23 13:09:23 richi Exp $
 *
 * Copyright (C) 2001, 2004 Richard Guenther
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

#include "glscript.h"
#ifdef __cplusplus
extern "C" {
#endif

/* NOTE, that for GLAME config space access to work you need to have
 * initialized glscript.
 */


/* Initialize the GLAME config space from its on-disk representation.
 * This does not fail as at least a default configuration is created. */
void glame_config_load();

/* Syncs the in-memory GLAME config space with the on-disk representation. */
void glame_config_sync();


/* Get the config SCM pair associated with the provided key (key . value).
 * If there is nothing associated with the key, a new empty node
 * is created and returned. Use this for reading and writing to
 * the GLAME config space. Returns #unspecified on error. */
SCM glame_config_get(const char *key, SCM s_default);
void glame_config_set(const char *key, SCM s_value);


/* Simple functions for reading/writing string/long/double values
 * from/into GLAME config space. 
 * Note that for glame_config_get_string a newly allocated string
 * is returned that you have to free yourself.
 */

void glame_config_set_string(const char *key, const char *value);
int glame_config_get_string(const char *key, char **value);
char *glame_config_get_string_with_default(const char *key, const char *def);

void glame_config_set_long(const char *key, long value);
int glame_config_get_long(const char *key, long *value);
long glame_config_get_long_with_default(const char *key, long def);

void glame_config_set_double(const char *key, double value);
int glame_config_get_double(const char *key, double *value);
double glame_config_get_double_with_default(const char *key, double def);

#ifdef __cplusplus
}
#endif
#endif
