/*
 * glsimd.c
 * $Id: glsimd.c,v 1.1 2001/03/05 15:04:07 richi Exp $
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

#include "glsimd.h"


static void c_interleaveUSHORT(struct iovec *dest, long cnt, ...);
static void c_deinterleaveUSHORT(struct iovec *source, long cnt, ...);
static void c_SAMPLE2USHORT(gl_u16 *dest, struct iovec *source,
                            long from, long size);


/* The simd operations table. Statically initialized to the C variants
 * to allow use without glsimd_init(). */
struct glsimd_ops_table glsimd = {
	interleaveUSHORT: c_interleaveUSHORT,
	deinterleaveUSHORT: c_deinterleaveUSHORT,
	SAMPLE2USHORT: c_SAMPLE2USHORT,
};


void glsimd_init(int force_c)
{
        /* Init with C only operations, i.e. safe default. */
	glsimd.interleaveUSHORT = c_interleaveUSHORT;
	glsimd.deinterleaveUSHORT = c_deinterleaveUSHORT;
	glsimd.SAMPLE2USHORT = c_SAMPLE2USHORT;

        /* Forced C only operations? */
	if (force_c)
		return;

	/* FIXME: now we would need to
         * 1. detect hardware capabilities
         * 2. benchmark(!?) alternatives
         * 3. select the best
         */
	/* For now (and testing) its compile-time. */
#if defined X86_3DNOW
        /* nothing... */
#elif defined X86_ISSE1
        /* nothing... */
#endif
}


static void c_interleaveUSHORT(struct iovec *dest, long cnt, ...)
{
}

static void c_deinterleaveUSHORT(struct iovec *source, long cnt, ...)
{
}

static void c_SAMPLE2USHORT(gl_u16 *dest, struct iovec *source,
                            long from, long size)
{
}

