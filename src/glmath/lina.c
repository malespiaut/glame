/*
 * <lina.c>
 *
 * Convenience library for common mathematical routines on audio samples.
 *
 * $Id: lina.c,v 1.2 2000/02/22 10:26:23 nold Exp $
 *
 * Copyright (C) 2000 Daniel Kobras
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

#include <math.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <glame_types.h>
#include "glmath.h"


void glm_print_vector(SAMPLE vec[], unsigned int n)
{
	int i;

	printf("(");
	for(i=0; i < n; i++)
		printf("%f, ", vec[i]);

	printf("\b\b)\n");
}

void glm_print_matrix(SAMPLE mat[][], unsigned int n)
{
	SAMPLE (*a)[n] = mat;
	unsigned int i, j;

	for(i=0; i < n; i++) {
		for(j=0; j < n; j++)
			printf("%f\t", a[i][j]);
		printf("\n");
	}
	printf("\n");
}

/* Solve linear equation a[i][j]*x[j] = b[i] for x[j].
 * a[][] is a quadratic matrix of dimension n x n,
 * x[], b[] are vectors of dimension n.
 *
 * NOTE: In the __ version, a and b get modified!
 *
 * Return value: 0 if x[] is a valid return value, -1 on error.
 *               
 */

int glm_solve_linear(SAMPLE *x, SAMPLE a[][], SAMPLE *b, unsigned int n)
{
	int ret=-1;
	SAMPLE *ac, *bc;

	ac=(SAMPLE *)malloc(n*n*sizeof(SAMPLE));
	bc=(SAMPLE *)malloc(n*sizeof(SAMPLE));

	if(!ac || !bc)
		goto _err_out;

	memcpy(ac, a, n*n*sizeof(SAMPLE));
	memcpy(bc, b, n*sizeof(SAMPLE));

	ret = __glm_solve_linear(x, ac, bc, n);
	
_err_out:
	free(bc);
	free(ac);
	return ret;
}

int __glm_solve_linear(SAMPLE *x, SAMPLE a[][], SAMPLE *b, unsigned int n)
{
	SAMPLE (*m)[n] = a;
	unsigned int row, col, i;

	if(!n) return -1;

	/* XXX: Usually the largest element in a column is taken as
         * pivot - I assume audio data doesn't cover too large a 
         * range, so we save the additional search pass.
	 */

	for(row = 0; row < n; row++) {
		if(m[row][row] == (SAMPLE)0) {
			SAMPLE tmp = b[row];
			i=row;
			while(++i < n)
				if(m[i][row] != (SAMPLE)0)
					break;
			if(i == n)
				return -1;
			b[row] = b[i];
			b[i] = tmp;
			for(col = row; col < n; col++) {
				SAMPLE tmp=m[row][col];
				m[row][col] = m[i][col];
				m[i][col]=tmp;
			}
		}
		b[row] /= m[row][row];
		for(col = row+1; col < n; col++) {
			m[row][col] /= m[row][row];
			for(i = row+1; i < n; i++)
				m[i][col] -= m[row][col]*m[i][row];
		}
		for(i = row+1; i < n; i++)
			b[i] -= b[row]*m[i][row];
	}
	/* We have a Jordan triangle now with diagonal elements all 1,
	 * elements below all zero. We didn't bother setting those
	 * values in m[i][j] though!
	 */
	row = n-1;
	do {
		x[row] = b[row];
		for(col = n-1; col > row; col--)
			x[row] -= m[row][col]*x[col];
	} while(row--);

	return 0;
}

/* Invert n x n matrix mat[i][j].
 *
 * NOTE: mat gets modified!
 *
 * Return value: 0 if inv[][] is a valid return value, -1 on error.
 */

int glm_invert_matrix(SAMPLE inv[][], SAMPLE mat[][], unsigned int n)
{
	SAMPLE (*a)[n] = mat, (*b)[n] = inv;
	unsigned int row, col, i, j;

	if(!n) return -1;
	
	memset(inv, 0, n*n*sizeof(SAMPLE));
	i = n-1;
	do {
		b[i][i] = (SAMPLE)1;
	} while(i--);

	for(row = 0; row < n; row++) {
		if(a[row][row] == (SAMPLE)0) {
			i=row;
			while(++i < n)
				if(a[i][row] != (SAMPLE)0)
					break;
			if(i == n)
				return -1;
			for(col = 0; col < n; col++) {
				SAMPLE tmp = b[row][col];
				b[row][col] = b[i][col];
				b[i][col] = tmp;
			}
			for(col = row; col < n; col++) {
				SAMPLE tmp=a[row][col];
				a[row][col] = a[i][col];
				a[i][col]=tmp;
			}
		}
		for(j=0; j < n; j++)
			b[row][j] /= a[row][row];
		for(col = row+1; col < n; col++) {
			a[row][col] /= a[row][row];
			for(i = row+1; i < n; i++)
				a[i][col] -= a[row][col]*a[i][row];
		}
		for(i = row+1; i < n; i++) {
			for(j=0; j < n; j++)
				b[i][j] -= b[row][j]*a[i][row];
		}
	}
	/* We have a Jordan triangle now with diagonal elements all 1,
	 * elements below all zero. We didn't bother setting those
	 * values in a[i][j] though!
	 */
	row = n-1;
	do {
		for(col = n-1; col > row; col--)
			for(j=0; j < n; j++)
				b[row][j] -= a[row][col]*b[col][j];
	} while(row--);

	return 0;
}


/*
 * Return coefficients for interpolating polynom of order n, satisfying
 * a[n]*x[j]^n + a[n-1]*x[j]^(n-1) + ... + a[0] = b[j] for all j in [0;n]
 *
 * NOTE: a[], x[]. and b[] must all be arrays of dimension n+1!
 * 
 * Return value: 0 if a[] is valid, -1 on error.
 */


int glm_poly_interpolate(SAMPLE *a, SAMPLE *x, SAMPLE *b, unsigned int n)
{

	unsigned int i, j;
	int ret;
	SAMPLE (*mat)[n+1];
	
	if(!n) {
		a[0] = b[0];
		return 0;
	}
	
	mat = (SAMPLE *)malloc((n+1) * (n+1) * sizeof(SAMPLE));
	if(!mat)
		return -1;

	for(i = 0; i <= n; i++) {
		mat[i][0] = (SAMPLE)1;
		for(j=1; j <= n; j++) {
			mat[i][j] = mat[i][j-1]*x[i];
		}
	}
	ret = glm_solve_linear(a, mat, b, n+1);
	free(mat);

	return ret;
}

/*
 * Evaluate polynom of order n, a[n]*x^n + a[n-1]*x^(n-1) + ... + a[0]
 * 
 * NOTE: a[] is supposed to be an array of size n+1!
 */

SAMPLE glm_poly_evaluate(SAMPLE *a, SAMPLE x, unsigned int n)
{
	SAMPLE ret;

	/* Using Horner's scheme to evaluate. */

	ret = a[n];
	while(n--) {
		ret *= x;
		ret += a[n];
	}
	return ret;
}
