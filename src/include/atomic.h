#ifndef _ATOMIC_H
#define _ATOMIC_H

/*
 * atomic.h
 *
 * Copyright (C) 2000, 2001 Daniel Kobras
 *
 * $Id: atomic.h,v 1.14 2004/10/23 13:09:27 richi Exp $
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

#if !defined NO_ASM && defined CPU_X86
#include "atomic_x86.h"
#elif !defined NO_ASM && defined CPU_MIPS && defined HAVE_MIPS_LL_SC
#include "atomic_mips.h"
#elif !defined NO_ASM && defined CPU_PPC
#include "atomic_ppc.h"
#elif !defined NO_ASM && defined CPU_ALPHA
#include "atomic_alpha.h"
#else
/*
 * The generic C versions of atomic operations in this file are safe but slow. 
 * If possible, we use the much more efficient assembler versions in 
 * architecture dependent headers. It is highly recommended to drop in
 * an asm version if you port to a new platform. [dk]
 * We now for space and simplicity reasons use one global mx to protect
 * atomic counters. Be sure to initialize this object early. [richi]
 */

#include <pthread.h>


typedef struct {
	volatile int	cnt;
} glame_atomic_t;

/* this is defined in glmid/glmid.c */
extern pthread_mutex_t glame_atomic_mx;

#define ATOMIC_INIT(a, val) do{ \
	(a).cnt = (val); \
} while(0)

#define ATOMIC_RELEASE(a) do {} while (0)

#define ATOMIC_VAL(a) ((a).cnt)


static inline void atomic_set(glame_atomic_t *a, int val)
{
	pthread_mutex_lock(&glame_atomic_mx);
	a->cnt = val;
	pthread_mutex_unlock(&glame_atomic_mx);
}

static inline int atomic_read(glame_atomic_t *a)
{
	int val;

	pthread_mutex_lock(&glame_atomic_mx);
        val = a->cnt;
	pthread_mutex_unlock(&glame_atomic_mx);

	return val;
}

static inline void atomic_inc(glame_atomic_t *a)
{
	pthread_mutex_lock(&glame_atomic_mx);
	a->cnt++;
	pthread_mutex_unlock(&glame_atomic_mx);
}

static inline void atomic_dec(glame_atomic_t *a)
{
	pthread_mutex_lock(&glame_atomic_mx);
	a->cnt--;
	pthread_mutex_unlock(&glame_atomic_mx);
}

static inline int atomic_dec_and_test(glame_atomic_t *a)
{
	int val;
	
	pthread_mutex_lock(&glame_atomic_mx);
	val = --(a->cnt);
	pthread_mutex_unlock(&glame_atomic_mx);
	
	return val == 0;
}

static inline void atomic_add(int val, glame_atomic_t *a)
{
	pthread_mutex_lock(&glame_atomic_mx);
	a->cnt += val;
	pthread_mutex_unlock(&glame_atomic_mx);
}

static inline void atomic_sub(int val, glame_atomic_t *a)
{
	pthread_mutex_lock(&glame_atomic_mx);
	a->cnt -= val;
	pthread_mutex_unlock(&glame_atomic_mx);
}

#endif
#endif
