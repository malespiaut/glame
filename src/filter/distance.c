/*
 * distance.c
 *
 * Copyright (C) 2000 Jim Garrison
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


/*
 * Returns distance when given coordinates in "dim" dimensions
 */

#include <stdarg.h>
#include <math.h>

extern double distance(unsigned dim, ...)
{
	va_list ap;
	double f, d = 0.0;
	int i;

	va_start(ap, dim);
	for (i = 0; i < dim; i++) {
		f = va_arg(ap, double);
		d += (f*f);
	}
	va_end(ap);

	return sqrt(d);
}
