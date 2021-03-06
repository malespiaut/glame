#ifndef _GLAME_TYPES_H
#define _GLAME_TYPES_H

/*
 * glame_types.h
 * $Id: glame_types.h,v 1.22 2004/10/23 13:09:27 richi Exp $
 * Copyright (C) 2000, 2001 Alexander Ehlert, Richard Guenther, Daniel Kobras
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

/* The recommended buffer size is GLAME_WBUFSIZE, the minimum
 * and maximum allowed sizes are GLAME_MIN_BUFSIZE and GLAME_MAX_BUFSIZE.
 * These numbers are specified in number of samples. */
extern int _GLAME_WBUFSIZE;
#define GLAME_MIN_BUFSIZE (GLAME_WBUFSIZE/4)
#define GLAME_MAX_BUFSIZE (GLAME_WBUFSIZE*4)
#define GLAME_BULK_BUFSIZE (64*1024)

#define GLAME_DEFAULT_SAMPLERATE 44100

/* SAMPLE is defined by config.h now.
 * typedef float SAMPLE; */
#define SAMPLE_SIZE ((long)sizeof(SAMPLE))

/* Sigh! Why is there no portable standard for those types? */
typedef   signed char	gl_s8;	/* Hope this covers 'char is unsigned' case. */
typedef unsigned char	gl_u8;
#if SIZEOF_SHORT == 2
typedef   signed short	gl_s16;
typedef unsigned short	gl_u16;
#elif SIZEOF_INT == 2
typedef   signed int	gl_s16;
typedef unsigned int	gl_u16;
#else
#error No 16 bit data type available.
#endif
#if SIZEOF_INT == 4
typedef   signed int	gl_s32;
typedef unsigned int	gl_u32;
#elif SIZEOF_LONG == 4
typedef   signed long	gl_s32;
typedef unsigned long	gl_u32;
#else
#error No 32 bit data type available.
#endif
/* Add when needed. Beware: long long is a GNU extension! */
#if SIZEOF_LONG == 8
typedef   signed long	gl_s64;
typedef unsigned long	gl_u64;
#elif SIZEOF_LONG_LONG == 8
typedef   signed long long	gl_s64;
typedef unsigned long long	gl_u64;
#else
#error No 64 bit data type available.
#endif 

#endif

