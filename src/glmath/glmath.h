/*
 * <glmath.h>
 *
 * Convenience library for common mathematical routines on audio samples.
 *
 * $Id: glmath.h,v 1.1 2000/02/21 11:00:14 nold Exp $
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

#include <glame_types.h>

/* Return square norm of a dim-dimensional vector. */

double glm_norm(unsigned int dim, double *vec);

/* Convenience functions, mainly for debugging... */

void glm_print_vector(SAMPLE vec[], unsigned int n);
void glm_print_matrix(SAMPLE mat[][], unsigned int n);

/* Solve linear equation a[i][j]*x[j] = b[i] for x[j].
 * a[][] is a quadratic matrix of dimension n x n,
 * x[], b[] are vectors of dimension n.
 *
 * NOTE: In the __ version, a and b get modified!
 *
 * Return value: 0 if x[] is a valid return value, -1 on error.
 *               
 */

int glm_solve_linear(SAMPLE *x, SAMPLE a[][], SAMPLE *b, unsigned int n);
int __glm_solve_linear(SAMPLE *x, SAMPLE a[][], SAMPLE *b, unsigned int n);

/* Invert n x n matrix mat[i][j].
 *
 * NOTE: mat gets modified!
 *
 * Return value: 0 if inv[][] is a valid return value, -1 on error.
 */

int glm_invert_matrix(SAMPLE inv[][], SAMPLE mat[][], unsigned int n);

/*
 * Return coefficients for interpolating polynom of order n, satisfying
 * a[n]*x[j]^n + a[n-1]*x[j]^(n-1) + ... + a[0] = b[j] for all j in [0;n]
 *
 * NOTE: a[], x[]. and b[] must all be arrays of dimension n+1!
 * 
 * Return value: 0 if a[] is valid, -1 on error.
 */


int glm_poly_interpolate(SAMPLE *a, SAMPLE *x, SAMPLE *b, unsigned int n);

/*
 * Evaluate polynom of order n, a[n]*x^n + a[n-1]*x^(n-1) + ... + a[0]
 * 
 * NOTE: a[] is supposed to be an array of size n+1!
 */

SAMPLE glm_poly_evaluate(SAMPLE *a, SAMPLE x, unsigned int n);

