#ifndef _UTIL_H
#define _UTIL_H

/*
 * util.h
 *
 * $Id: util.h,v 1.10 2000/10/09 08:37:59 richi Exp $
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

#ifndef	HAVE_GCC
#define __PRETTY_FUNCTION__	"Unknown location" 
#define __attribute__(x)
#endif

static void __glame_do_panic(const char *, const char *,
			     const char *) __attribute__((__noreturn__)) __attribute__((__unused__));
static void __glame_do_panic(const char *file, const char *func,
			     const char *msg)
{
	fprintf(stderr, "\nPANIC in file %s, function %s:\n%s\n",
		file, func, msg);
	perror("errno says");
	*((int *)0) = 0;
	exit(1);
}
#define PANIC(msg) __glame_do_panic(__FILE__, __PRETTY_FUNCTION__, msg)

#ifndef NDEBUG
#define DERROR(msg) PANIC(msg)
#else
#define DERROR(msg)
#endif

#ifdef HAVE_GCC
# ifdef DEBUG
#define DPRINTF(msg, args...) printf(__PRETTY_FUNCTION__ ": " msg , ## args)
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

#endif
