#ifndef _SIMD_C_H
#define _SIMD_C_H

/*
 * simd_c.h
 *
 * Copyright (C) 2001 Richard Guenther
 *
 * $Id: simd_c.h,v 1.1 2001/01/25 09:16:39 richi Exp $
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
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

/*
 * Generic C versions of simd operations.
 * If possible, we use the much more efficient assembler versions in 
 * architecture dependent headers. It is highly recommended to drop in
 * an asm version if you port to a new platform.
 */


/* SAMPLE to various type conversion including clipping of the
 * samples to [-1,1].
 * - SAMPLE to signed short
 * - SAMPLE to unsigned short
 * - signed short to SAMPLE
 * - unsigned short to SAMPLE
 * - SAMPLE to signed char
 * - SAMPLE to unsigned char
 * - signed char to SAMPLE
 * - unsigned char to SAMPLE
 */

#ifndef HAVE_ARCH_SAMPLE2SHORT
static inline gl_s16 SAMPLE2SHORT(SAMPLE s)
{
        return (gl_s16)((s<-1.0 ? -1.0 : (s>1.0 ? 1.0 : s))
		*(s<0.0 ? (1<<15) : (1<<15)-1));
}
#endif

#ifndef HAVE_ARCH_SAMPLE2USHORT
static inline gl_u16 SAMPLE2USHORT(SAMPLE s)
{	
	s += 1.0, s *= 0.5;
	return (gl_u16)((s<0.0 ? 0.0 : (s>1.0 ? 1.0 : s))*((1<<16)-1));
}
#endif

#ifndef HAVE_ARCH_SHORT2SAMPLE
#define SHORT2SAMPLE(s)  ((SAMPLE)(gl_s16)(s)/(SAMPLE)(1<<15))
#endif

#ifndef HAVE_ARCH_USHORT2SAMPLE
#define USHORT2SAMPLE(s) ((SAMPLE)(gl_u16)(s)/(SAMPLE)(1<<15) - 1.0)
#endif

#ifndef HAVE_ARCH_SAMPLE2CHAR
static inline gl_s8 SAMPLE2CHAR(SAMPLE s)
{
        return (gl_s8)((s<-1.0 ? -1.0 : (s>1.0 ? 1.0 : s))
		*(s<0.0 ? (1<<7) : (1<<7)-1));
}
#endif

#ifndef HAVE_ARCH_SAMPLE2UCHAR
static inline gl_u8 SAMPLE2UCHAR(SAMPLE s)
{
	s += 1.0, s *= 0.5;
	return (gl_u8)((s<0.0 ? 0.0 : (s>1.0 ? 1.0 : s))*((1<<7)-1));
}
#endif

#ifndef HAVE_ARCH_CHAR2SAMPLE
#define CHAR2SAMPLE(s)  ((SAMPLE)(gl_s8)(s)/(SAMPLE)(1<<7))
#endif

#ifndef HAVE_ARCH_UCHAR2SAMPLE
#define UCHAR2SAMPLE(s) ((SAMPLE)(gl_u8)(s)/(SAMPLE)(1<<7) - 1.0)
#endif




/* Here follows a set of fast computing macros for standard operations.
 * To be implemented using ISSE/3DNOW stuff if available. Probably only
 * the higher count ones (SCALARPROD_XD_4).
 * Note that if called like SCALARPROD_1D_1(s, s, f) the compiler
 * can optimize away the destp++ test. Asm versions will want to do
 * seperate versions of both cases.
 */

/* Generic scalarproduct operations in 1D, 2D and 3D.
 */

#ifndef HAVE_ARCH_SCALARPROD_1D_1
#define SCALARPROD_1D_1(destp, source1p, fact1) \
do { \
	*destp = *(source1p++)*fact1; \
        if (&destp != &source1p) destp++; \
} while (0)
#endif
#ifndef HAVE_ARCH_SCALARPROD_1D_4
#define SCALARPROD_1D_4(destp, source1p, fact1) \
do { \
        SCALARPROD_1D_1(destp, source1p, fact1); \
        SCALARPROD_1D_1(destp, source1p, fact1); \
        SCALARPROD_1D_1(destp, source1p, fact1); \
        SCALARPROD_1D_1(destp, source1p, fact1); \
} while (0)
#endif

#ifndef HAVE_ARCH_SCALARPROD_2D_1
#define SCALARPROD_2D_1(destp, source1p, source2p, fact1, fact2) \
do { \
	*destp = *(source1p++)*fact1 + *(source2p++)*fact2; \
	if (&destp != &source1p) destp++; \
} while (0)
#endif
#ifndef HAVE_ARCH_SCALARPROD_2D_4
#define SCALARPROD_2D_4(destp, source1p, source2p, fact1, fact2) \
do { \
        SCALARPROD_2D_1(destp, source1p, source2p, fact1, fact2); \
        SCALARPROD_2D_1(destp, source1p, source2p, fact1, fact2); \
        SCALARPROD_2D_1(destp, source1p, source2p, fact1, fact2); \
        SCALARPROD_2D_1(destp, source1p, source2p, fact1, fact2); \
} while (0)
#endif

#ifndef HAVE_ARCH_SCALARPROD_3D_1
#define SCALARPROD_3D_1(destp, source1p, source2p, source3p, fact1, fact2, fact3) \
do { \
	*destp = *(source1p++)*fact1 + *(source2p++)*fact2 + *(source3p++)*fact3; \
	if (&destp != &source1p) destp++; \
} while (0)
#endif
#ifndef HAVE_ARCH_SCALARPROD_3D_4
#define SCALARPROD_3D_4(destp, source1p, source2p, source3p, fact1, fact2, fact3) \
do { \
        SCALARPROD_3D_1(destp, source1p, source2p, source3p, fact1, fact2, fact3); \
        SCALARPROD_3D_1(destp, source1p, source2p, source3p, fact1, fact2, fact3); \
        SCALARPROD_3D_1(destp, source1p, source2p, source3p, fact1, fact2, fact3); \
        SCALARPROD_3D_1(destp, source1p, source2p, source3p, fact1, fact2, fact3); \
} while (0)
#endif


/* Inversion of values. X = -X. */

#ifndef HAVE_ARCH_INVERT1
#define INVERT1(destsourcep) \
do { \
	*destsourcep = -*destsourcep; \
	destsourcep++; \
} while (0)
#endif
#ifndef HAVE_ARCH_INVERT4
#define INVERT4(destsourcep) \
do { \
        INVERT1(destsourcep); \
        INVERT1(destsourcep); \
        INVERT1(destsourcep); \
        INVERT1(destsourcep); \
} while (0)
#endif


/* Simple addition. */

#ifndef HAVE_ARCH_ADD1
#define ADD1(destsourcep,sum) \
do { \
	*destsourcep++ += sum; \
} while (0)
#endif

#ifndef HAVE_ARCH_ADD4
#define ADD4(destsourcep,sum) \
do { \
        ADD1(destsourcep, sum); \
        ADD1(destsourcep, sum); \
        ADD1(destsourcep, sum); \
        ADD1(destsourcep, sum); \
} while (0)
#endif


#endif
