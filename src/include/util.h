#ifndef _UTIL_H
#define _UTIL_H

/*
 * util.h
 *
 * $Id: util.h,v 1.16 2002/02/15 13:05:18 nold Exp $
 *
 * Copyright (C) 1999, 2000 Richard Guenther
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

#include <sys/param.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

#if !defined HAVE_GCC || defined __cplusplus
#define __PRETTY_FUNCTION__ __FILE__
#endif
#if !defined HAVE_GCC
#define __attribute__(x)
#endif

#if !defined HAVE_GCC || defined __cplusplus
static void __glame_do_panic(const char *file, int line, const char *msg)
{
	fprintf(stderr, "\nPANIC in %s::%i\n%s\n", file, line, msg);
	perror("errno says");
	*((int *)0) = 0;
	exit(1);
}
#define PANIC(msg) __glame_do_panic(__FILE__, __LINE__, msg)

#else
static void __glame_do_panic(const char *, const char *, int, const char *) __attribute__((__noreturn__)) __attribute__((__unused__));
static void __glame_do_panic(const char *file, const char *func, int line, const char *msg)
{
	fprintf(stderr, "\nPANIC in %s: %s::%i\n%s\n", file, func, line, msg);
	perror("errno says");
	*((int *)0) = 0;
	exit(1);
}
#define PANIC(msg) __glame_do_panic(__FILE__, __PRETTY_FUNCTION__, __LINE__, msg)
#endif

#ifndef NDEBUG
#define DERROR(msg) PANIC(msg)
#else
#define DERROR(msg) 
#endif

/* 
 * Some evil macros require DERROR to eval to an rvalue. Don't use for
 * anything sane!
 */
#ifndef NDEBUG
#define DERROR_eval(msg) (PANIC(msg), 0==1)
#else
#define DERROR_eval(msg) (0==1)
#endif

#ifdef HAVE_GCC
# ifdef DEBUG
#define DPRINTF(msg, args...) printf("%s: " msg, __PRETTY_FUNCTION__ , ## args)
# else
#define DPRINTF(msg, args...)
# endif
#else
# ifdef DEBUG
static inline void DPRINTF(const char *templ, ...)
{
	va_list	args;
	va_start(args, templ);
	vprintf(templ, args);
	va_end(args);
}
# else
static inline void DPRINTF(const char *templ, ...)
{
}
# endif
#endif

#ifndef MIN
#define MIN(a, b) ((a)<(b)?(a):(b))
#endif

#ifndef MAX
#define MAX(a, b) ((a)<(b)?(b):(a))
#endif

/* alloc zeroed mem, malloc/calloc syntax. */
#define ALLOC(type) (type *)calloc(1, sizeof(type))
#define ALLOCN(n, type) (n == 0 ? NULL : (type *)calloc((n), sizeof(type)))

/* stuff. */
#ifndef HAVE_SQRTF
#define sqrtf(x) ((float) sqrt((float) (x)))
#endif

#ifndef HAVE_SINF
#define sinf(x) ((float) sin((float) (x)))
#endif

#endif
