/*
 * $Id: atomic_x86.h,v 1.7 2000/10/28 13:42:31 richi Exp $
 *
 * Most of the code was taken from include/asm-i386/atomic.h from the
 * Linux kernel source code and adapted to the need of GLAME.
 */

#ifndef _ATOMIC_X86_H
#define _ATOMIC_X86_H

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

static inline void atomic_add(int i, volatile glame_atomic_t *v)
{
	__asm__ __volatile__(
		"lock ; addl %1,%0"
		:"=m" (v->counter)
		:"ir" (i), "m" (v->counter));
}

static inline void atomic_sub(int i, volatile glame_atomic_t *v)
{
	__asm__ __volatile__(
		"lock ; subl %1,%0"
		:"=m" (v->counter)
		:"ir" (i), "m" (v->counter));
}

static inline void atomic_inc(volatile glame_atomic_t *v)
{
	__asm__ __volatile__(
		"lock ; incl %0"
		:"=m" (v->counter)
		:"m" (v->counter));
}

static inline void atomic_dec(volatile glame_atomic_t *v)
{
	__asm__ __volatile__(
		"lock ; decl %0"
		:"=m" (v->counter)
		:"m" (v->counter));
}

static inline int atomic_dec_and_test(volatile glame_atomic_t *v)
{
        unsigned char c;

        __asm__ __volatile__(
                "lock ; decl %0; sete %1"
                :"=m" (v->counter), "=qm" (c)
                :"m" (v->counter) : "memory");
        return c != 0;
}


#endif
