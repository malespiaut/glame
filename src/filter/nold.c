/*
 * nold.c
 * $Id: nold.c,v 1.8 2000/02/21 17:48:05 nold Exp $
 *
 * Copyright (C) 2000 Daniel Kobras
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

/*
 * This is nold's personal playground. Stuff in here is strictly 
 * confidential, under permantent development and probably highly
 * broken. You have been warned.
 */
 
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <unistd.h>
#include <stdlib.h>
#include "filter.h"
#include "util.h"
#include "glame_types.h"
#include <limits.h>

/* Native SGI audio output used to live here. It's now merged into 
 * audio_io.c
 */

int nold_register()
{
	return 0;
}




