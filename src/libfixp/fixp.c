/*********************************************
 * fixp.c
 *
 * Helper lib for generic fixed point
 * arithmetics.
 *
 * $Id: fixp.c,v 1.2 2000/01/24 10:22:52 richi Exp $
 *********************************************/

#ifndef __FIXP_LIB__
#define __FIXP_LIB__
#endif

#include <fixp.h>

int fixp_bias_shift	= FIXP_DEFAULT_BIAS_SHIFT;
int fixp_bias		= 1 << FIXP_DEFAULT_BIAS_SHIFT;
int fixp_mul_lshift	= (FIXP_DEFAULT_BIAS_SHIFT >> 1) + (FIXP_DEFAULT_BIAS_SHIFT & 1);
int fixp_mul_rshift	= FIXP_DEFAULT_BIAS_SHIFT & 1;

void fixp_set_bias(int bias_shift)
{
	fixp_bias_shift = bias_shift;
	fixp_bias = 1<<bias_shift;
	fixp_mul_lshift=(fixp_bias_shift>>1) + (fixp_bias_shift&1);
	fixp_mul_rshift=fixp_bias_shift&1;
}
