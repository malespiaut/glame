#ifndef _GLAME_TYPES_H
#define _GLAME_TYPES_H

/*
 * glame_types.h
 * $Id: glame_types.h,v 1.12 2000/03/22 10:15:45 richi Exp $
 * Copyright (C) 2000 Alexander Ehlert, Richard Guenther
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

/* internal SAMPLE format and size.
 * this should be changable w/o any code breakage!
 */

#define GLAME_WBUFSIZE 16384
#define GLAME_MIN_BUFSIZE 512
#define GLAME_MAX_BUFSIZE (64*1024)
#define GLAME_DEFAULT_SAMPLERATE 44100

typedef float SAMPLE;
#define SAMPLE_SIZE sizeof(SAMPLE)

typedef struct midi_event {
	char flags;
	unsigned int ts;	/* timestamp */
	unsigned char b[3];	/* 1 status and 2 data bytes */
} midi_event_t;


#endif

