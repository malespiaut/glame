/*
 * vec.c
 *
 * $Id: vec.c,v 1.1 2000/02/21 11:00:14 nold Exp $
 * 
 * Copyright (C) 2000 Jim Garrison, Daniel Kobras
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

#include <math.h>
#include <glame_types.h>
#include "glmath.h"

/*
 * Returns length of vector vec in "dim" dimensions
 * (common square norm)
 */

double glm_norm(unsigned int dim, double *vec)
{
	double d = 0.0;
	int i;

	for (i = 0; i < dim; i++) 
		d += vec[i]*vec[i];
	
	return sqrt(d);
}
