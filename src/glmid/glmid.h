#ifndef _GLMID_H
#define _GLMID_H

#include "glplugin.h"
#include "gltrack.h"


int builtins_register();
int glscript_init();


static inline int glmid_init(int start_scheme)
{
	if (builtins_register() == -1)
		return -1;
	if (start_scheme && glscript_init() == -1)
		return -1;
	return 0;
}

#endif
