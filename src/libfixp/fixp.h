/*********************************************
 * fixp.h
 *
 * Helper lib for generic fixed point
 * arithmetics.
 *
 * $Id: fixp.h,v 1.1 2000/01/20 14:54:19 richi Exp $
 *********************************************/

/*
 * This library is totally broken and does probably not
 * what you want it to do. Ugh. Didn't try to fix it
 * becaus I dont want to rewrite it from scratch which
 * would be necessary to do so. Ficken.
 * [richi]
 * Fixed. [dk]
 */

#ifndef _FIXP_H
#define _FIXP_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

/* FIXME: Add autoconf rules some day */
/* change to your preference */
typedef unsigned int fixp_t;
#define nFIXP_IS_SIGNED	
#define FIXP_DEFAULT_BIAS_SHIFT	16
/* no user maintainable parts below */


#define FIXP_T_MAX	(~(fixp_t)0)
#ifdef  FIXP_IS_SIGNED
#define FIXP_MIN	FIXP_T_MAX
#define FIXP_MAX	(-(FIXP_MIN-1))
#else
#define FIXP_MIN	0
#define FIXP_MAX	FIXP_T_MAX
#endif

#define FIXP_WIDTH	(sizeof(fixp_t)<<3) 

extern int fixp_bias_shift;
extern int fixp_bias;
extern int fixp_mul_lshift;
extern int fixp_mul_rshift;

void		fixp_set_bias(int);
void		fixp_clamp(void);
void		fixp_wrap(void);
inline fixp_t	float2fixp(float);
inline float	fixp2float(fixp_t);
inline fixp_t	fixp_adc(fixp_t, fixp_t);
inline fixp_t	fixp_adw(fixp_t, fixp_t);
inline fixp_t	fixp_suc(fixp_t, fixp_t);
inline fixp_t	fixp_suw(fixp_t, fixp_t);
inline fixp_t	fixp_muc(fixp_t, fixp_t);
inline fixp_t	fixp_muw(fixp_t, fixp_t);
inline fixp_t	fixp_div(fixp_t, fixp_t);

#ifdef __FIXP_LIB__

extern fixp_t float2fixp(float);
extern float  fixp2float(fixp_t);
extern fixp_t fixp_adc(fixp_t, fixp_t);
extern fixp_t fixp_adw(fixp_t, fixp_t);
extern fixp_t fixp_suc(fixp_t, fixp_t);
extern fixp_t fixp_suw(fixp_t, fixp_t);
extern fixp_t fixp_muc(fixp_t, fixp_t);
extern fixp_t fixp_muw(fixp_t, fixp_t);
extern fixp_t fixp_div(fixp_t, fixp_t);

#else

inline fixp_t float2fixp(float f)
{
	return (f*fixp_bias);
}

inline float fixp2float(fixp_t x)
{
	return ((float)(x)/fixp_bias);
}

inline fixp_t fixp_adc(fixp_t x, fixp_t y)
{
	fixp_t res;

	res = x + y;

#ifdef FIXP_IS_SIGNED
	if(y>0) {
#endif
		if(res<x)
			return FIXP_MAX;
#ifdef FIXP_IS_SIGNED
	} else {
		if(res>x)
			return FIXP_MIN;
	}
#endif
	return res;
}

inline fixp_t fixp_adw(fixp_t x, fixp_t y)
{
	return x + y;
}

inline fixp_t fixp_suc(fixp_t x, fixp_t y)
{
	fixp_t res;

	res = x - y;

#ifdef FIXP_IS_SIGNED
	if(y>0) {
#endif
		if(res>x)
			return FIXP_MIN;
#ifdef FIXP_IS_SIGNED
	} else {
		if(res<x)
			return FIXP_MAX;
	}
#endif
	return res;
}

inline fixp_t fixp_suw(fixp_t x, fixp_t y)
{
	return x - y;
}

inline fixp_t fixp_muc(fixp_t x, fixp_t y)
{
	/* TODO */
	return 123456789;
}

inline fixp_t fixp_muw(fixp_t x, fixp_t y)
{
	return (((x>>fixp_mul_lshift) * (y>>fixp_mul_lshift))
			<<fixp_mul_rshift);
}

inline fixp_t fixp_div(fixp_t x, fixp_t y)
{
	/* fixed point division sucks rocks. */
	int i = fixp_bias_shift;
	fixp_t res;

	res = x/y;
	if(res)
		x-=y*res;
	
	while(i--) {
		x<<=1;
		res<<=1;
		if(x>=y) {
			res|=1;
			x-=y;
		}
	}
	return res;
}

#endif
#endif
