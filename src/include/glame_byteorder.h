#ifndef _GLAME_BYTEORDER_H
#define _GLAME_BYTEORDER_H

/*
 * glame_byteorder.h
 * $Id: glame_byteorder.h,v 1.1 2000/04/03 02:37:40 nold Exp $
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

#include <byteswap.h>
#include "glame_types.h"

/* We follow the Linux kernel style of defining endianness macros and functions.
 * The plain versions are inline functions that can be referenced as pointers,
 * the versions preceded by __ are macros. The 'p' versions take a pointer
 * argument.
 */

#ifdef WORDS_BIGENDIAN
#define __gl_le64_to_cpu(x) bswap_64((x))
#define __gl_le32_to_cpu(x) bswap_32((x))
#define __gl_le16_to_cpu(x) bswap_16((x))
#define __gl_be64_to_cpu(x) (x)
#define __gl_be32_to_cpu(x) (x)
#define __gl_be16_to_cpu(x) (x)

#else	/* Little endian follows */

#define __gl_be64_to_cpu(x) bswap_64((x))
#define __gl_be32_to_cpu(x) bswap_32((x))
#define __gl_be16_to_cpu(x) bswap_16((x))
#define __gl_le64_to_cpu(x) (x)
#define __gl_le32_to_cpu(x) (x)
#define __gl_le16_to_cpu(x) (x)

#endif	/* WORDS_BIGENDIAN */

#define __gl_cpu_to_le64(x) __gl_le64_to_cpu((x))
#define __gl_cpu_to_le32(x) __gl_le32_to_cpu((x))
#define __gl_cpu_to_le16(x) __gl_le16_to_cpu((x))
#define __gl_cpu_to_be64(x) __gl_be64_to_cpu((x))
#define __gl_cpu_to_be32(x) __gl_be32_to_cpu((x))
#define __gl_cpu_to_be16(x) __gl_be16_to_cpu((x))

/* No 64 bit types so far */
#if 0
#define __gl_le64_to_cpup(x) __gl_le64_to_cpu(*(gl_u64 *)((x)))
#define __gl_be64_to_cpup(x) __gl_be64_to_cpu(*(gl_u64 *)((x))) 
#define __gl_cpu_to_le64p(x) __gl_cpu_to_le64(*(gl_u64 *)((x)))
#define __gl_cpu_to_be64p(x) __gl_cpu_to_be64(*(gl_u64 *)((x)))
#endif

#define __gl_le32_to_cpup(x) __gl_le32_to_cpu(*(gl_u32 *)((x)))
#define __gl_le16_to_cpup(x) __gl_le16_to_cpu(*(gl_u16 *)((x)))
#define __gl_be32_to_cpup(x) __gl_be32_to_cpu(*(gl_u32 *)((x)))
#define __gl_be16_to_cpup(x) __gl_be16_to_cpu(*(gl_u16 *)((x)))
#define __gl_cpu_to_le32p(x) __gl_cpu_to_le32(*(gl_u32 *)((x)))
#define __gl_cpu_to_le16p(x) __gl_cpu_to_le16(*(gl_u16 *)((x)))
#define __gl_cpu_to_be32p(x) __gl_cpu_to_be32(*(gl_u32 *)((x)))
#define __gl_cpu_to_be16p(x) __gl_cpu_to_be16(*(gl_u16 *)((x)))

/* No 64 bit types so far */
#if 0
static inline gl_u64 gl_le64_to_cpu(gl_u64 x)
{
	return __gl_le64_to_cpu(x);
}
static inline gl_u64 gl_le64_to_cpup(gl_u64 *x)
{
	return __gl_le64_to_cpup(x);
}
static inline gl_u64 gl_be64_to_cpu(gl_u64 x)
{
	return __gl_be64_to_cpu(x);
}
static inline gl_u64 gl_be64_to_cpup(gl_u64 *x)
{
	return __gl_be64_to_cpup(x);
}
static inline gl_u64 gl_cpu_to_le64(gl_u64 x)
{
	return __gl_cpu_to_le64(x);
}
static inline gl_u64 gl_cpu_to_le64p(gl_u64 *x)
{
	return __gl_cpu_to_le64p(x);
}
static inline gl_u64 gl_cpu_to_be64(gl_u64 x)
{
	return __gl_cpu_to_be64(x);
}
static inline gl_u64 gl_cpu_to_be64p(gl_u64 *x)
{
	return __gl_cpu_to_be64p(x);
}
#endif
static inline gl_u32 gl_le32_to_cpu(gl_u32 x)
{
	return __gl_le32_to_cpu(x);
}
static inline gl_u32 gl_le32_to_cpup(gl_u32 *x)
{
	return __gl_le32_to_cpup(x);
}
static inline gl_u32 gl_be32_to_cpu(gl_u32 x)
{
	return __gl_be32_to_cpu(x);
}
static inline gl_u32 gl_be32_to_cpup(gl_u32 *x)
{
	return __gl_be32_to_cpup(x);
}
static inline gl_u16 gl_le16_to_cpu(gl_u16 x)
{
	return __gl_le16_to_cpu(x);
}
static inline gl_u16 gl_le16_to_cpup(gl_u16 *x)
{
	return __gl_le16_to_cpup(x);
}
static inline gl_u16 gl_be16_to_cpu(gl_u16 x)
{
	return __gl_be16_to_cpu(x);
}
static inline gl_u16 gl_be16_to_cpup(gl_u16 *x)
{
	return __gl_be16_to_cpup(x);
}
static inline gl_u32 gl_cpu_to_le32(gl_u32 x)
{
	return __gl_cpu_to_le32(x);
}
static inline gl_u32 gl_cpu_to_le32p(gl_u32 *x)
{
	return __gl_cpu_to_le32p(x);
}
static inline gl_u32 gl_cpu_to_be32(gl_u32 x)
{
	return __gl_cpu_to_be32(x);
}
static inline gl_u32 gl_cpu_to_be32p(gl_u32 *x)
{
	return __gl_cpu_to_be32p(x);
}
static inline gl_u16 gl_cpu_to_le16(gl_u16 x)
{
	return __gl_cpu_to_le16(x);
}
static inline gl_u16 gl_cpu_to_le16p(gl_u16 *x)
{
	return __gl_cpu_to_le16p(x);
}
static inline gl_u16 gl_cpu_to_be16(gl_u16 x)
{
	return __gl_cpu_to_be16(x);
}
static inline gl_u16 gl_cpu_to_be16p(gl_u16 *x)
{
	return __gl_cpu_to_be16p(x);
}

#endif

