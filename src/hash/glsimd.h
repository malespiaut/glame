#ifndef _GLSIMD_H
#define _GLSIMD_H

/*
 * glsimd.h
 * $Id: glsimd.h,v 1.4 2001/04/11 08:37:59 richi Exp $
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

#include <sys/types.h>
#include <sys/uio.h>
#include "glame_types.h"


/* SIMD operations through a virtual function table containing
 * run-time configurable highly optimized code when operating on
 * medium to large size data.
 */
struct glsimd_ops_table {
	/* Various variants of scalar products, i.e.
	 *   result[i] = f1*c1[i] + f2*c2[i] + ...
	 * cnt vector pairs fi/ci are multiplied and the result is
	 * stored in cnt scalars in result. For the non determined
	 * dimension variant scalar_product_Nd dim specifies the
	 * dimension. */
	void (*scalar_product_1d)(SAMPLE *result, long cnt,
				  SAMPLE *c1, SAMPLE f1);
	void (*scalar_product_1dI)(SAMPLE *result_c1,  long cnt, SAMPLE f1);
	void (*scalar_product_2d)(SAMPLE *result, long cnt,
				  SAMPLE *c1, SAMPLE f1,
				  SAMPLE *c2, SAMPLE f2);
	void (*scalar_product_2dI)(SAMPLE *result_c1,  long cnt, SAMPLE f1,
				   SAMPLE *c2, SAMPLE f2);
	void (*scalar_product_3d)(SAMPLE *result, long cnt,
				  SAMPLE *c1, SAMPLE f1,
				  SAMPLE *c2, SAMPLE f2,
				  SAMPLE *c3, SAMPLE f3);
	void (*scalar_product_3dI)(SAMPLE *result_c1,  long cnt, SAMPLE f1,
				   SAMPLE *c2, SAMPLE f2,
				   SAMPLE *c3, SAMPLE f3);
	void (*scalar_product_Nd)(SAMPLE *result, long cnt,
				  SAMPLE **c1, SAMPLE *f1, long dim);

#if 0 /* Not yet implemented. */
	/* Routines we commonly need for things like audio and file io.
	 * These routines do endian conversion, if necessary. As s16
	 * formats are generally either mono or interleaved stereo, two
	 * versions of the routines exist. */
	void (*sample_to_s16_le_M)(SAMPLE *source, void *dest, long cnt);
	void (*sample_to_s16_be_M)(SAMPLE *source, void *dest, long cnt);
	void (*s16_le_to_sample_M)(void *source, SAMPLE *dest, long cnt);
	void (*s16_be_to_sample_M)(void *source, SAMPLE *dest, long cnt);
	void (*sample_to_s16_le_S)(SAMPLE *left, SAMPLE *right,
				   void *dest, long cnt);
	void (*sample_to_s16_be_S)(SAMPLE *left, SAMPLE *right,
				   void *dest, long cnt);
	void (*s16_le_to_sample_S)(void *source,
				   SAMPLE *left, SAMPLE *right, long cnt);
	void (*s16_be_to_sample_S)(void *source,
				   SAMPLE *left, SAMPLE *right, long cnt);
#endif

        /* Add other stuff as needed. */
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



#endif
