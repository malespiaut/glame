/*
 * Atomic operations that C can't guarantee us.  Useful for
 * resource counting etc..
 *
 * But use these as seldom as possible since they are much more slower
 * than regular operations.
 *
 * This file is subject to the terms and conditions of the GNU General Public
 * License.  See the file "COPYING" in the main directory of this archive
 * for more details.
 *
 * Copyright (C) 1996, 1997 by Ralf Baechle
 *
 */
#ifndef __ASM_MIPS_ATOMIC_H
#define __ASM_MIPS_ATOMIC_H

typedef struct { volatile int counter; } glame_atomic_t;

#define INIT_GLAME_ATOMIC_T (glame_atomic_t) { 0 }

#define ATOMIC_INIT(a, val) do { (a).counter = (val); } while(0)

#define ATOMIC_RELEASE(a)

#define ATOMIC_VAL(v) ((v).counter)
#define atomic_read(v)	((v)->counter)
#define atomic_set(v,i)	((v)->counter = (i))

/*
 * Make sure gcc doesn't try to be clever and move things around
 * on us. We need to use _exactly_ the address the user gave us,
 * not some alias that contains the same information.
 */
#define __atomic_fool_gcc(x) (*(volatile struct { int a[100]; } *)x)

static inline void atomic_add(int i, volatile glame_atomic_t * v)
{
	unsigned long temp;

	__asm__ __volatile__(
		"1:\tll\t%0,%1\n\t"
		"addu\t%0,%2\n\t"
		"sc\t%0,%1\n\t"
		"beqz\t%0,1b"
		:"=&r" (temp),
		 "=m" (__atomic_fool_gcc(v))
		:"Ir" (i),
		 "m" (__atomic_fool_gcc(v)));
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
		 "=m" (__atomic_fool_gcc(v))
		:"Ir" (i),
		 "m" (__atomic_fool_gcc(v)));
}

#define atomic_inc(v) atomic_add(1,(v))
#define atomic_dec(v) atomic_sub(1,(v))


#endif /* __ASM_MIPS_ATOMIC_H */

