/*
 * glsimd.c
 * $Id: glsimd.c,v 1.10 2005/03/20 19:25:40 richi Exp $
 *
 * Copyright (C) 2001, 2002 Richard Guenther
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
static void
c_scalar_product_1d(SAMPLE * restrict result, long cnt,
		    SAMPLE * restrict c1, SAMPLE f1);
static void
c_scalar_product_1dI(SAMPLE * restrict result_c1, long cnt, SAMPLE f1);
static void
c_scalar_product_2d(SAMPLE * restrict result, long cnt,
		    SAMPLE * restrict c1, SAMPLE f1,
		    SAMPLE * restrict c2, SAMPLE f2);
static void
c_scalar_product_2dI(SAMPLE * restrict result_c1, long cnt, SAMPLE f1,
		     SAMPLE * restrict c2, SAMPLE f2);
static void
c_scalar_product_3d(SAMPLE * restrict result, long cnt,
		    SAMPLE * restrict c1, SAMPLE f1,
		    SAMPLE * restrict c2, SAMPLE f2,
		    SAMPLE * restrict c3, SAMPLE f3);
static void
c_scalar_product_3dI(SAMPLE * restrict result_c1, long cnt, SAMPLE f1,
		     SAMPLE * restrict c2, SAMPLE f2,
		     SAMPLE * restrict c3, SAMPLE f3);
static void
c_scalar_product_Nd(SAMPLE * restrict result, long cnt,
		    SAMPLE * restrict *c, SAMPLE * restrict f, long dim);


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

	/* FIXME: now we should
         * 1. detect hardware capabilities
         * 2. benchmark(!?) alternatives
         * 3. select the best
         */
}

static void
c_scalar_product_1d(SAMPLE * restrict result, long cnt,
		    SAMPLE * restrict c1, SAMPLE f1)
{
	int i;
	for (i=0; i<cnt; ++i)
		result[i] = f1 * c1[i];
}
static void
c_scalar_product_1dI(SAMPLE * restrict result_c1, long cnt, SAMPLE f1)
{
	int i;
	for (i=0; i<cnt; ++i)
		result_c1[i] *= f1;
}
static void
c_scalar_product_2d(SAMPLE * restrict result, long cnt,
		    SAMPLE * restrict c1, SAMPLE f1,
		    SAMPLE * restrict c2, SAMPLE f2)
{
	int i;
	for (i=0; i<cnt; ++i)
		result[i] = f1 * c1[i]
			+ f2 * c2[i];
}
static void
c_scalar_product_2dI(SAMPLE * restrict result_c1, long cnt, SAMPLE f1,
		     SAMPLE * restrict c2, SAMPLE f2)
{
	int i;
	for (i=0; i<cnt; ++i)
		result_c1[i] = f1 * result_c1[i]
			+ f2 * c2[i];
}
static void
c_scalar_product_3d(SAMPLE * restrict result, long cnt,
		    SAMPLE * restrict c1, SAMPLE f1,
		    SAMPLE * restrict c2, SAMPLE f2,
		    SAMPLE * restrict c3, SAMPLE f3)
{
	int i;
	for (i=0; i<cnt; ++i)
		result[i] = f1 * c1[i]
			+ f2 * c2[i]
			+ f3 * c3[i];
}
static void
c_scalar_product_3dI(SAMPLE * restrict result_c1, long cnt, SAMPLE f1,
		     SAMPLE * restrict c2, SAMPLE f2,
		     SAMPLE * restrict c3, SAMPLE f3)
{
	int i;
	for (i=0; i<cnt; ++i)
		result_c1[i] = f1 * result_c1[i]
			+ f2 * c2[i]
			+ f3 * c3[i];
}
static void
c_scalar_product_Nd(SAMPLE * restrict result, long cnt,
		    SAMPLE * restrict *c, SAMPLE * restrict f, long dim)
{
	int i,j;

	switch (dim) {
	case 0:
		memset(result, 0, cnt*sizeof(SAMPLE));
		return;
	case 1:
		glsimd.scalar_product_1d(result, cnt, c[0], f[0]);
		return;
	case 2:
		glsimd.scalar_product_2d(result, cnt,
					 c[0], f[0], c[1], f[1]);
		return;
	case 3:
		glsimd.scalar_product_3d(result, cnt, c[0], f[0],
					 c[1], f[1], c[2], f[2]);
		return;
	default:
		/* Fall through.  */
		;
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
