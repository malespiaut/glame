#ifndef _ATOMIC_MIPS_H
#define _ATOMIC_MIPS_H

/*
 * $Id: atomic_mips.h,v 1.8 2004/10/23 13:09:27 richi Exp $
 *
 * Copyright (C) 2000, 2001 Daniel Kobras, Richard Guenther
 * 
 * Most of the code was taken from include/asm-mips/atomic.h from the
 * Linux kernel source code and adapted to the need of GLAME.
 * The kernel's MIPS atomic.h is
 * 
 * Copyright (C) 1996, 1997 by Ralf Baechle
 *
 * This file is subject to the terms and conditions of the GNU General Public
 * License.  See the file "COPYING" in the main directory of this archive
 * for more details.
 */

/* Atomic operations that C can't guarantee us.  Useful for
 * resource counting etc..
 *
 * But use these as seldom as possible since they are much more slower
 * than regular operations.
 */

/*
 * Make sure gcc doesn't try to be clever and move things around
 * on us. We need to use _exactly_ the address the user gave us,
 * not some alias that contains the same information.
 */
typedef struct { volatile int counter; } glame_atomic_t;

#define ATOMIC_INIT(a, val) do { (a).counter = (val); } while(0)

#define ATOMIC_RELEASE(a)

#define ATOMIC_VAL(v) ((v).counter)
#define atomic_read(v)	((v)->counter)
#define atomic_set(v,i)	((v)->counter = (i))


static inline void atomic_add(int i, volatile glame_atomic_t * v)
{
	unsigned long temp;

	__asm__ __volatile__(
		"1:\tll\t%0,%1\n\t"
		"addu\t%0,%2\n\t"
		"sc\t%0,%1\n\t"
		"beqz\t%0,1b"
		:"=&r" (temp),
		 "=m" (v->counter)
		:"Ir" (i),
		 "m" (v->counter));
}

static inline void atomic_sub(int i, volatile glame_atomic_t * v)
{
	unsigned long temp;

	__asm__ __volatile__(
		"1:\tll\t%0,%1\n\t"
		"subu\t%0,%2\n\t"
		"sc\t%0,%1\n\t"
		"beqz\t%0,1b"
		:"=&r" (temp),
		 "=m" (v->counter)
		:"Ir" (i),
		 "m" (v->counter));
}

static inline int atomic_sub_return(int i, glame_atomic_t * v)
{
        unsigned long temp, result;

        __asm__ __volatile__(
                ".set\tnoreorder\n"
                "1:\tll\t%1,%2\n\t"
                "subu\t%0,%1,%3\n\t"
                "sc\t%0,%2\n\t"
                "beqz\t%0,1b\n\t"
                "subu\t%0,%1,%3\n\t"
                ".set\treorder"
                :"=&r" (result),
                 "=&r" (temp),
                 "=m" (v->counter)
                :"Ir" (i),
                 "m" (v->counter)
		:"memory");

        return result;
}


#define atomic_inc(v) atomic_add(1,(v))
#define atomic_dec(v) atomic_sub(1,(v))
#define atomic_dec_and_test(v) (atomic_sub_return(1, (v)) == 0)

#endif /* __ASM_MIPS_ATOMIC_H */
