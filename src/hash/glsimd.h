#ifndef _GLSIMD_H
#define _GLSIMD_H

/*
 * glsimd.h
 * $Id: glsimd.h,v 1.1 2001/03/05 15:04:07 richi Exp $
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

#include <sys/uio.h>
#include "glame_types.h"


/* SIMD operations through a virtual function table containing
 * run-time configurable highly optimized code when operating on
 * medium to large size data.
 */
struct glsimd_ops_table {
        /* From the SAMPLE buffers (struct iovec * in ..., NULL
         * terminated) produce an interleaved buffer in dest
         * with room for cnt samples (each channel) using
         * unsigned shorts as sample type. */
	void (*interleaveUSHORT)(struct iovec *dest, long cnt, ...);
        /* Symmetric to interleaveUSHORT */
	void (*deinterleaveUSHORT)(struct iovec *source, long cnt, ...); 
	/* Add other types as needed. */

        /* Convert size SAMPLEs starting at position from in the
         * iovec source to unsigned shorts, storing them into dest. */
	void (*SAMPLE2USHORT)(gl_u16 *dest, struct iovec *source,
                              long from, long size);
        /* Add other types as needed. */
};

/* SIMD operations table filled by glsimd_init(), for use by arbitrary
 * modules. */
extern struct glsimd_ops_table glsimd;


/* Initialize the SIMD operations table with the optimal routines.
 * Specifying force_c != 0 forces the C versions of each operation. */
void glsimd_init(int force_c);



/* Inline functions and macros for generic audio data operations.
 * Use if you are operating on single data items rather than blocks
 * of data.
 * Note that you generally should use these instead of trying to
 * be clever yourself as these are considered to be the correct(TM)
 * versions.
 */

#define SHORT2SAMPLE(s)  ((SAMPLE)(gl_s16)(s)/(SAMPLE)(1<<15))
#define USHORT2SAMPLE(s) ((SAMPLE)(gl_u16)(s)/(SAMPLE)(1<<15) - 1.0)
#define CHAR2SAMPLE(s)  ((SAMPLE)(gl_s8)(s)/(SAMPLE)(1<<7))
#define UCHAR2SAMPLE(s) ((SAMPLE)(gl_u8)(s)/(SAMPLE)(1<<7) - 1.0)

static inline gl_s16 SAMPLE2SHORT(SAMPLE s)
{
        return (gl_s16)((s<-1.0 ? -1.0 : (s>1.0 ? 1.0 : s))
                *(s<0.0 ? (1<<15) : (1<<15)-1));
}

static inline gl_u16 SAMPLE2USHORT(SAMPLE s)
{       
        s += 1.0, s *= 0.5;
        return (gl_u16)((s<0.0 ? 0.0 : (s>1.0 ? 1.0 : s))*((1<<16)-1));
}

static inline gl_s8 SAMPLE2CHAR(SAMPLE s)
{
        return (gl_s8)((s<-1.0 ? -1.0 : (s>1.0 ? 1.0 : s))
                *(s<0.0 ? (1<<7) : (1<<7)-1));
}

static inline gl_u8 SAMPLE2UCHAR(SAMPLE s)
{
        s += 1.0, s *= 0.5;
        return (gl_u8)((s<0.0 ? 0.0 : (s>1.0 ? 1.0 : s))*((1<<7)-1));
}



/* Stuff that got "merged" from old simd_*.h / filter_tools.h but is to
 * be phased out. Dont use in new code. You have been warned.
 */

#define SCALARPROD_1D_1(destp, source1p, fact1) \
do { \
        *destp = *(source1p++)*fact1; \
        if (&destp != &source1p) destp++; \
} while (0)
#define SCALARPROD_1D_4(destp, source1p, fact1) \
do { \
        SCALARPROD_1D_1(destp, source1p, fact1); \
        SCALARPROD_1D_1(destp, source1p, fact1); \
        SCALARPROD_1D_1(destp, source1p, fact1); \
        SCALARPROD_1D_1(destp, source1p, fact1); \
} while (0)

#define SCALARPROD_2D_1(destp, source1p, source2p, fact1, fact2) \
do { \
        *destp = *(source1p++)*fact1 + *(source2p++)*fact2; \
        if (&destp != &source1p) destp++; \
} while (0)
#define SCALARPROD_2D_4(destp, source1p, source2p, fact1, fact2) \
do { \
        SCALARPROD_2D_1(destp, source1p, source2p, fact1, fact2); \
        SCALARPROD_2D_1(destp, source1p, source2p, fact1, fact2); \
        SCALARPROD_2D_1(destp, source1p, source2p, fact1, fact2); \
        SCALARPROD_2D_1(destp, source1p, source2p, fact1, fact2); \
} while (0)

#define SCALARPROD_3D_1(destp, source1p, source2p, source3p, fact1, fact2, fact3) \
do { \
        *destp = *(source1p++)*fact1 + *(source2p++)*fact2 + *(source3p++)*fact3; \
        if (&destp != &source1p) destp++; \
} while (0)
#define SCALARPROD_3D_4(destp, source1p, source2p, source3p, fact1, fact2, fact3) \
do { \
        SCALARPROD_3D_1(destp, source1p, source2p, source3p, fact1, fact2, fact3); \
        SCALARPROD_3D_1(destp, source1p, source2p, source3p, fact1, fact2, fact3); \
        SCALARPROD_3D_1(destp, source1p, source2p, source3p, fact1, fact2, fact3); \
        SCALARPROD_3D_1(destp, source1p, source2p, source3p, fact1, fact2, fact3); \
} while (0)


#define INVERT1(destsourcep) \
do { \
        *destsourcep = -*destsourcep; \
        destsourcep++; \
} while (0)
#define INVERT4(destsourcep) \
do { \
        INVERT1(destsourcep); \
        INVERT1(destsourcep); \
        INVERT1(destsourcep); \
        INVERT1(destsourcep); \
} while (0)


#define ADD1(destsourcep,sum) \
do { \
        *destsourcep++ += sum; \
} while (0)
#define ADD4(destsourcep,sum) \
do { \
        ADD1(destsourcep, sum); \
        ADD1(destsourcep, sum); \
        ADD1(destsourcep, sum); \
        ADD1(destsourcep, sum); \
} while (0)


#endif
