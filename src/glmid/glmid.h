#ifndef _GLMID_H
#define _GLMID_H


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


#endif
