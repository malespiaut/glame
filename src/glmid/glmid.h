#ifndef _GLMID_H
#define _GLMID_H

#include "filter.h"
#include "glplugin.h"


/* Initializes all glame subsystems. Returns 0 on success,
 * -1 on error. */
int glame_init();

/* Initializes all glame subsystems, including the scheme
 * scripting part using guile, if available. Returns 0 on
 * success, -1 on error.
 * Due to weirdness of guiles initialization procedure you
 * have to provide a main function that will be executed, if
 * everything went right - glame_init_with_guile does not
 * return until main exits. */
int glame_init_with_guile(void (*main)(void));


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


#endif
