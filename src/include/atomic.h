#ifndef _ATOMIC_H
#define _ATOMIC_H

/*
 * atomic.h
 *
 * Copyright (C) 2000 Daniel Kobras
 *
 * $Id: atomic.h,v 1.3 2000/02/09 11:01:26 richi Exp $
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
 *
 * Assembler code taken from the Linux Kernel which is Copyright
 * by Linus Torvalds and others.
 */

/*
 * Generic version of atomic operations. We might want to make them more
 * efficient some day using platform and/or compiler dependent assembly
 * versions. [dk]
 */

#include <pthread.h>

#if defined HAVE_GCC && defined CPU_X86
#include "atomic_x86.h"
#elif defined HAVE_GCC && defined CPU_MIPS
#include "atomic_mips.h"
#else

typedef struct {
	pthread_mutex_t mx;
	volatile int	cnt;
} glame_atomic_t;

#define INIT_GLAME_ATOMIC_T \
		(glame_atomic_t) { PTHREAD_MUTEX_INITIALIZER, 0 }

#define ATOMIC_INIT(a, val) do{ \
	(a) = INIT_GLAME_ATOMIC_T; \
	(a).cnt = (val); \
} while(0)

#define ATOMIC_RELEASE(a) pthread_mutex_destroy(&(a).mx)

#define ATOMIC_VAL(a) ((a).cnt)

static inline void atomic_set(glame_atomic_t *a, int val)
{
	pthread_mutex_lock(&a->mx);
	a->cnt = val;
	pthread_mutex_unlock(&a->mx);
}

static inline int atomic_read(glame_atomic_t *a)
{
	int val;

	pthread_mutex_lock(&a->mx);
        val = a->cnt;
	pthread_mutex_unlock(&a->mx);

	return val;
}

static inline void atomic_inc(glame_atomic_t *a)
{
	pthread_mutex_lock(&a->mx);
	a->cnt++;
	pthread_mutex_unlock(&a->mx);
}

static inline void atomic_dec(glame_atomic_t *a)
{
	pthread_mutex_lock(&a->mx);
	a->cnt--;
	pthread_mutex_unlock(&a->mx);
}

static inline void atomic_add(glame_atomic_t *a, int val)
{
	pthread_mutex_lock(&a->mx);
	a->cnt += val;
	pthread_mutex_unlock(&a->mx);
}

static inline void atomic_sub(glame_atomic_t *a, int val)
{
	pthread_mutex_lock(&a->mx);
	a->cnt -= val;
	pthread_mutex_unlock(&a->mx);
}

#endif
#endif
