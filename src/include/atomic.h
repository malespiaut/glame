#ifndef _ATOMIC_H
#define _ATOMIC_H

/*
 * atomic.h
 *
 * Copyright (C) 2000 Daniel Kobras
 *
 * $Id: atomic.h,v 1.8 2000/04/11 12:22:50 nold Exp $
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
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <pthread.h>

/*
 * The generic C versions of atomic operations in this file are safe but slow. 
 * If possible, we use the much more efficient assembler versions in 
 * architecture dependent headers. It is highly recommended to drop in
 * an asm version if you port to a new platform. [dk]
 */

#if defined CPU_X86
#include "atomic_x86.h"
#elif defined CPU_MIPS
#include "atomic_mips.h"
#else

typedef struct {
	pthread_mutex_t mx;
	volatile int	cnt;
} glame_atomic_t;

#define ATOMIC_INIT(a, val) do{ \
	pthread_mutex_init(&(a).mx, NULL); \
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

static inline int atomic_dec_and_test(glame_atomic_t *a)
{
	int val;
	
	pthread_mutex_lock(&a->mx);
	val = --(a->cnt);
	pthread_mutex_unlock(&a->mx);
	
	return val == 0;
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
