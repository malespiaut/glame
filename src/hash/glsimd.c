/*
 * glsimd.c
 * $Id: glsimd.c,v 1.5 2003/04/23 20:55:09 richi Exp $
 *
 * Copyright (C) 2001 Richard Guenther
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>
#include "glsimd.h"


/* Forward declarations. */
void c_scalar_product_1d(SAMPLE *result, long cnt,
			 SAMPLE *c1, SAMPLE f1);
void c_scalar_product_1dI(SAMPLE *result_c1, long cnt, SAMPLE f1);
void c_scalar_product_2d(SAMPLE *result, long cnt,
			 SAMPLE *c1, SAMPLE f1,
			 SAMPLE *c2, SAMPLE f2);
void c_scalar_product_2dI(SAMPLE *result_c1, long cnt, SAMPLE f1,
			  SAMPLE *c2, SAMPLE f2);
void c_scalar_product_3d(SAMPLE *result, long cnt,
			 SAMPLE *c1, SAMPLE f1,
			 SAMPLE *c2, SAMPLE f2,
			 SAMPLE *c3, SAMPLE f3);
void c_scalar_product_3dI(SAMPLE *result_c1, long cnt, SAMPLE f1,
			  SAMPLE *c2, SAMPLE f2,
			  SAMPLE *c3, SAMPLE f3);
void c_scalar_product_Nd(SAMPLE *result, long cnt,
			 SAMPLE **c, SAMPLE *f, long dim);


/* The simd operations table. Statically initialized to the C variants
 * to allow use without glsimd_init(). */
struct glsimd_ops_table glsimd = {
	scalar_product_1d: c_scalar_product_1d,
	scalar_product_1dI: c_scalar_product_1dI,
	scalar_product_2d: c_scalar_product_2d,
	scalar_product_2dI: c_scalar_product_2dI,
	scalar_product_3d: c_scalar_product_3d,
	scalar_product_3dI: c_scalar_product_3dI,
	scalar_product_Nd: c_scalar_product_Nd
};


void glsimd_init(int force_c)
{
        /* Init with C only operations, i.e. safe default. */
	glsimd.scalar_product_1d = c_scalar_product_1d;
	glsimd.scalar_product_1dI = c_scalar_product_1dI;
	glsimd.scalar_product_2d = c_scalar_product_2d;
	glsimd.scalar_product_2dI = c_scalar_product_2dI;
	glsimd.scalar_product_3d = c_scalar_product_3d;
	glsimd.scalar_product_3dI = c_scalar_product_3dI;
	glsimd.scalar_product_Nd = c_scalar_product_Nd;

        /* Forced C only operations? */
	if (force_c)
		return;

	/* Also, with SAMPLE == double, optimized versions will
	 * certainly not work. */
#ifndef SAMPLE_FLOAT
	return;
#endif

	/* FIXME: now we should
         * 1. detect hardware capabilities
         * 2. benchmark(!?) alternatives
         * 3. select the best
         */
}


void c_scalar_product_1d(SAMPLE *result, long cnt,
			 SAMPLE *c1, SAMPLE f1)
{
#ifdef HAVE_GCC_SIMD
	/* For different alignment of result/c1 and small cnt, do ordinary C. */
	if (cnt < 8*GLAME_4VECTOR_ALIGN/SAMPLE_SIZE
	    || (((long)result & ~(GLAME_4VECTOR_ALIGN-1))
	        != ((long)c1 & ~(GLAME_4VECTOR_ALIGN-1))))
#endif
	while (cnt--)
		*(result++) = f1 * *(c1++);
#ifdef HAVE_GCC_SIMD
	else {
		int pre = ((long)result & (GLAME_4VECTOR_ALIGN-1)) / SAMPLE_SIZE;
		int vcnt = (cnt - pre) / (GLAME_4VECTOR_ALIGN/SAMPLE_SIZE);
		int post = (cnt - pre) % (GLAME_4VECTOR_ALIGN/SAMPLE_SIZE);
		while (pre--)
			*(result++) = f1 * *(c1++);
		{
			float f1v[4] __attribute__((aligned(GLAME_4VECTOR_ALIGN))) = { f1, f1, f1, f1 };
			//const v4sf f1v = { f1, f1, f1, f1 };
			while (vcnt--) {
				*(v4sf *)result = *(v4sf *)f1v * *(v4sf *)c1;
				result += (GLAME_4VECTOR_ALIGN/SAMPLE_SIZE);
				c1 += (GLAME_4VECTOR_ALIGN/SAMPLE_SIZE);
			}
		}
		while (post--)
			*(result++) = f1 * *(c1++);
	}
#endif
}
void c_scalar_product_1dI(SAMPLE *result_c1, long cnt, SAMPLE f1)
{
	while (cnt--)
		*(result_c1++) *= f1;
}
void c_scalar_product_2d(SAMPLE *result, long cnt,
			 SAMPLE *c1, SAMPLE f1,
			 SAMPLE *c2, SAMPLE f2)
{
#ifdef HAVE_GCC_SIMD
	/* For different alignment of result/c1 and small cnt, do ordinary C. */
	if (cnt < 4*GLAME_4VECTOR_ALIGN/SAMPLE_SIZE
	    || (((long)result & ~(GLAME_4VECTOR_ALIGN-1))
	        != ((long)c1 & ~(GLAME_4VECTOR_ALIGN-1)))
	    || (((long)result & ~(GLAME_4VECTOR_ALIGN-1))
		!= ((long)c2 & ~(GLAME_4VECTOR_ALIGN-1))))
#endif
	while (cnt--)
		*(result++) = f1 * *(c1++)
			+ f2 * *(c2++);
#ifdef HAVE_GCC_SIMD
	else {
		int pre = ((long)result & (GLAME_4VECTOR_ALIGN-1)) / SAMPLE_SIZE;
		int vcnt = (cnt - pre) / (GLAME_4VECTOR_ALIGN/SAMPLE_SIZE);
		int post = (cnt - pre) % (GLAME_4VECTOR_ALIGN/SAMPLE_SIZE);
		while (pre--)
			*(result++) = f1 * *(c1++)
				+ f2 * *(c2++);
		{
			float f1v[4] __attribute__((aligned(GLAME_4VECTOR_ALIGN))) = { f1, f1, f1, f1 };
			float f2v[4] __attribute__((aligned(GLAME_4VECTOR_ALIGN))) = { f2, f2, f2, f2 };
			//const v4sf f1v = { f1, f1, f1, f1 };
			//const v4sf f2v = { f2, f2, f2, f2 };
			while (vcnt--) {
				*(v4sf *)result = *(v4sf *)f1v * *(v4sf *)c1
					+ *(v4sf *)f2v * *(v4sf *)c2;
				result += (GLAME_4VECTOR_ALIGN/SAMPLE_SIZE);
				c1 += (GLAME_4VECTOR_ALIGN/SAMPLE_SIZE);
				c2 += (GLAME_4VECTOR_ALIGN/SAMPLE_SIZE);
			}
		}
		while (post--)
			*(result++) = f1 * *(c1++)
				+ f2 * *(c2++);
	}
#endif
}
void c_scalar_product_2dI(SAMPLE *result_c1, long cnt, SAMPLE f1,
			  SAMPLE *c2, SAMPLE f2)
{
	while (cnt--) {
		*result_c1 = f1 * *result_c1
			+ f2 * *(c2++);
		result_c1++;
	}
}
void c_scalar_product_3d(SAMPLE *result, long cnt,
			 SAMPLE *c1, SAMPLE f1,
			 SAMPLE *c2, SAMPLE f2,
			 SAMPLE *c3, SAMPLE f3)
{
	while (cnt--)
		*(result++) = f1 * *(c1++)
			+ f2 * *(c2++)
			+ f3 * *(c3++);
}
void c_scalar_product_3dI(SAMPLE *result_c1, long cnt, SAMPLE f1,
			  SAMPLE *c2, SAMPLE f2,
			  SAMPLE *c3, SAMPLE f3)
{
	while (cnt--) {
		*result_c1 = f1 * *result_c1
			+ f2 * *(c2++)
			+ f3 * *(c3++);
		result_c1++;
	}
}
void c_scalar_product_Nd(SAMPLE *result, long cnt,
			 SAMPLE **c, SAMPLE *f, long dim)
{
	int i,j;

	if (dim == 0) {
		memset(result, 0, cnt*sizeof(SAMPLE));
		return;
	} else if (dim == 1) {
		glsimd.scalar_product_1d(result, cnt, c[0], f[0]);
		return;
	} else if (dim == 2) {
		glsimd.scalar_product_2d(result, cnt,
					 c[0], f[0], c[1], f[1]);
		return;
	} else if (dim == 3) {
		glsimd.scalar_product_3d(result, cnt, c[0], f[0],
					 c[1], f[1], c[2], f[2]);
		return;
	}
	for (i=0; i<cnt; i++) {
		/* Minimum is 4d now. */
		*result = f[0] * c[0][i]
			+ f[1] * c[1][i]
			+ f[2] * c[2][i]
			+ f[3] * c[3][i];
		for (j=4; j<dim; j++)
			*result += f[j] * c[j][i];
		result++;
	}
}
