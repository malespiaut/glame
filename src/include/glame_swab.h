#ifndef _LINUX_BYTEORDER_SWAB_H
#define _LINUX_BYTEORDER_SWAB_H

/*
 * linux/include/linux/byteorder/swab.h from the Linux kernel which is
 * Copyright (C) by Linus Torvalds and others
 *
 * Byte-swapping, independently from CPU endianness
 *	swabXX[ps]?(foo)
 *
 * Francois-Rene Rideau <fare@tunes.org> 19971205
 *    separated swab functions from cpu_to_XX,
 *    to clean up support for bizarre-endian architectures.
 *
 * See asm-i386/byteorder.h and suches for examples of how to provide
 * architecture-dependent optimized versions
 *
 * stripped to provide "basic working" byteswap for GLAME.
 * - Richard Guenther.
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

#include "glame_types.h"


/* casts are necessary for constants, because we never know how for sure
 * how U/UL/ULL map to gl_u16, gl_u32, gl_u64. At least not in a portable way.
 */
#ifndef bswap_16
#define bswap_16(x) \
	((gl_u16)( \
		(((gl_u16)(x) & (gl_u16)0x00ffU) << 8) | \
		(((gl_u16)(x) & (gl_u16)0xff00U) >> 8) ))
#endif
#ifndef bswap_32
#define bswap_32(x) \
	((gl_u32)( \
		(((gl_u32)(x) & (gl_u32)0x000000ffUL) << 24) | \
		(((gl_u32)(x) & (gl_u32)0x0000ff00UL) <<  8) | \
		(((gl_u32)(x) & (gl_u32)0x00ff0000UL) >>  8) | \
		(((gl_u32)(x) & (gl_u32)0xff000000UL) >> 24) ))
#endif
#ifndef bswap_64
#define bswap_64(x) \
	((gl_u64)( \
		(gl_u64)(((gl_u64)(x) & (gl_u64)0x00000000000000ffULL) << 56) | \
		(gl_u64)(((gl_u64)(x) & (gl_u64)0x000000000000ff00ULL) << 40) | \
		(gl_u64)(((gl_u64)(x) & (gl_u64)0x0000000000ff0000ULL) << 24) | \
		(gl_u64)(((gl_u64)(x) & (gl_u64)0x00000000ff000000ULL) <<  8) | \
	        (gl_u64)(((gl_u64)(x) & (gl_u64)0x000000ff00000000ULL) >>  8) | \
		(gl_u64)(((gl_u64)(x) & (gl_u64)0x0000ff0000000000ULL) >> 24) | \
		(gl_u64)(((gl_u64)(x) & (gl_u64)0x00ff000000000000ULL) >> 40) | \
		(gl_u64)(((gl_u64)(x) & (gl_u64)0xff00000000000000ULL) >> 56) ))
#endif

#endif /* _LINUX_BYTEORDER_SWAB_H */
