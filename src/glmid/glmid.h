#ifndef _GLMID_H
#define _GLMID_H

/*
 * glmid.h
 * $Id: glmid.h,v 1.12 2003/05/18 19:25:04 xwolf Exp $
 *
 * Copyright (C) 1999, 2000, 2001 Richard Guenther
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

#include "filter.h"
#include "glplugin.h"


#ifdef __cplusplus
extern "C" {
#endif

/* Initializes all glame subsystems, including the scheme
 * scripting part using guile, if available. Returns 0 on
 * success, -1 on error.
 * Due to weirdness of guiles initialization procedure you
 * have to provide a main function that will be executed, if
 * everything went right - glame_init_with_guile does not
 * return until main exits. */
int glame_init(void (*main)(void), int argc, char **argv);


/* Loads the plugin(s) out of the specified file and registers
 * them. Returns 0 on success and -1 on error. */
int glame_load_plugin(const char *fname);

/* Loads the plugin out of the specified file and creates and
 * returns one instance of it. Returns NULL error, does not
 * register a plugin. */
filter_t *glame_load_instance(const char *fname);

/* Creates and registers a new plugin with the specified name
 * and associates the specified filter with it. Returns NULL
 * on error and the registered plugin on success. */
plugin_t *glame_create_plugin(filter_t *filter, const char *name);

#ifdef __cplusplus
}
#endif

#endif
