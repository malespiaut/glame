#ifndef _ATOMIC_PPC_H
#define _ATOMIC_PPC_H

/*
 * $Id: atomic_ppc.h,v 1.2 2004/10/23 13:09:27 richi Exp $
 *
 * Copyright (C) 2001 Daniel Kobras, Richard Guenther
 *
 * This code was taken from linux/include/asm-ppc/atomic.h from the
 * Linux kernel source code and adapted to the need of GLAME.
 * The Linux kernel is
 * Copyright (C) by Linus Torvalds and others
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
 * Atomic operations that C can't guarantee us.  Useful for
 * resource counting etc..
 */

/*
 * Make sure gcc doesn't try to be clever and move things around
 * on us. We need to use _exactly_ the address the user gave us,
 * not some alias that contains the same information.
 */
typedef struct { volatile int counter; } glame_atomic_t;

#define ATOMIC_INIT(a, val) do { (a).counter = (val); } while(0)

#define ATOMIC_RELEASE(a)

#define ATOMIC_VAL(a)		((a).counter)

#define atomic_read(v)          ((v)->counter)
#define atomic_set(v,i)		(((v)->counter) = (i))

static inline int atomic_add_return(int a, glame_atomic_t *v)
{
        int t;

        __asm__ __volatile__("\n\
1:      lwarx   %0,0,%3\n\
        add     %0,%2,%0\n\
        stwcx.  %0,0,%3\n\
        bne-    1b"
        : "=&r" (t), "=m" (v->counter)
        : "r" (a), "r" (v), "m" (v->counter)
        : "cc");

        return t;
}

static inline int atomic_sub_return(int a, glame_atomic_t *v)
{
        int t;

        __asm__ __volatile__("\n\
1:      lwarx   %0,0,%3\n\
        subf    %0,%2,%0\n\
        stwcx.  %0,0,%3\n\
        bne-    1b"
        : "=&r" (t), "=m" (v->counter)
        : "r" (a), "r" (v), "m" (v->counter)
        : "cc");

        return t;
}

static inline int atomic_inc_return(glame_atomic_t *v)
{
        int t;

        __asm__ __volatile__("\n\
1:      lwarx   %0,0,%2\n\
        addic   %0,%0,1\n\
        stwcx.  %0,0,%2\n\
        bne-    1b"
        : "=&r" (t), "=m" (v->counter)
        : "r" (v), "m" (v->counter)
        : "cc");

        return t;
}

static inline int atomic_dec_return(glame_atomic_t *v)
{
        int t;

        __asm__ __volatile__("\n\
1:      lwarx   %0,0,%2\n\
        addic   %0,%0,-1\n\
        stwcx.  %0,0,%2\n\
        bne     1b"
        : "=&r" (t), "=m" (v->counter)
        : "r" (v), "m" (v->counter)
        : "cc");

        return t;
}

#define atomic_add(a, v)                ((void) atomic_add_return((a), (v)))
#define atomic_sub(a, v)                ((void) atomic_sub_return((a), (v)))
#define atomic_inc(v)                   ((void) atomic_inc_return((v)))
#define atomic_dec(v)                   ((void) atomic_dec_return((v)))
#define atomic_dec_and_test(v)          (atomic_dec_return((v)) == 0)

#endif
