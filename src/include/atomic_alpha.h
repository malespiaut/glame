#ifndef _ATOMIC_ALPHA_H
#define _ATOMIC_ALPHA_H

/*
 * $Id: atomic_alpha.h,v 1.2 2004/10/23 13:09:27 richi Exp $
 *
 * Copyright (C) 2001 Daniel Kobras, Richard Guenther
 *
 * This code was taken from linux/include/asm-alpha/atomic.h from the
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

/*
 * To get proper branch prediction for the main line, we must branch
 * forward to code at the end of this object's .text section, then
 * branch back to restart the operation.
 */

static inline void atomic_add(int i, glame_atomic_t * v)
{
        unsigned long temp;
        __asm__ __volatile__(
        "1:     ldl_l %0,%1\n"
        "       addl %0,%2,%0\n"
        "       stl_c %0,%1\n"
        "       beq %0,2f\n"
        ".subsection 2\n"
        "2:     br 1b\n"
        ".previous"
        :"=&r" (temp), "=m" (v->counter)
        :"Ir" (i), "m" (v->counter));
}

static inline void atomic_sub(int i, glame_atomic_t * v)
{
        unsigned long temp;
        __asm__ __volatile__(
        "1:     ldl_l %0,%1\n"
        "       subl %0,%2,%0\n"
        "       stl_c %0,%1\n"
        "       beq %0,2f\n"
        ".subsection 2\n"
        "2:     br 1b\n"
        ".previous"
        :"=&r" (temp), "=m" (v->counter)
        :"Ir" (i), "m" (v->counter));
}

/*
 * Same as above, but return the result value
 */
static inline long atomic_add_return(int i, glame_atomic_t * v)
{
        long temp, result;
        __asm__ __volatile__(
        "1:     ldl_l %0,%1\n"
        "       addl %0,%3,%2\n"
        "       addl %0,%3,%0\n"
        "       stl_c %0,%1\n"
        "       beq %0,2f\n"
        "       mb\n"
        ".subsection 2\n"
        "2:     br 1b\n"
        ".previous"
        :"=&r" (temp), "=m" (v->counter), "=&r" (result)
        :"Ir" (i), "m" (v->counter) : "memory");
        return result;
}

static inline long atomic_sub_return(int i, glame_atomic_t * v)
{
        long temp, result;
        __asm__ __volatile__(
        "1:     ldl_l %0,%1\n"
        "       subl %0,%3,%2\n"
        "       subl %0,%3,%0\n"
        "       stl_c %0,%1\n"
        "       beq %0,2f\n"
        "       mb\n"
        ".subsection 2\n"
        "2:     br 1b\n"
        ".previous"
        :"=&r" (temp), "=m" (v->counter), "=&r" (result)
        :"Ir" (i), "m" (v->counter) : "memory");
        return result;
}

#define atomic_dec_and_test(v) (atomic_sub_return(1, (v)) == 0)
#define atomic_inc(v) atomic_add(1,(v))
#define atomic_dec(v) atomic_sub(1,(v))


#endif
