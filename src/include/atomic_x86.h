#ifndef __ARCH_I386_ATOMIC__
#define __ARCH_I386_ATOMIC__

/*
 * Atomic operations that C can't guarantee us.  Useful for
 * resource counting etc..
 */

/*
 * Make sure gcc doesn't try to be clever and move things around
 * on us. We need to use _exactly_ the address the user gave us,
 * not some alias that contains the same information.
 */
#define __atomic_fool_gcc(x) (*(volatile struct { int a[100]; } *)x)

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
		:"=m" (__atomic_fool_gcc(v))
		:"ir" (i), "m" (__atomic_fool_gcc(v)));
}

static inline void atomic_sub(int i, volatile glame_atomic_t *v)
{
	__asm__ __volatile__(
		"lock ; subl %1,%0"
		:"=m" (__atomic_fool_gcc(v))
		:"ir" (i), "m" (__atomic_fool_gcc(v)));
}

static inline void atomic_inc(volatile glame_atomic_t *v)
{
	__asm__ __volatile__(
		"lock ; incl %0"
		:"=m" (__atomic_fool_gcc(v))
		:"m" (__atomic_fool_gcc(v)));
}

static inline void atomic_dec(volatile glame_atomic_t *v)
{
	__asm__ __volatile__(
		"lock ; decl %0"
		:"=m" (__atomic_fool_gcc(v))
		:"m" (__atomic_fool_gcc(v)));
}


#endif

