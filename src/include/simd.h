#ifndef _SIMD_H
#define _SIMD_H

/*
 * simd.h
 *
 * Copyright (C) 2001 Richard Guenther
 *
 * $Id: simd.h,v 1.1 2001/01/25 09:16:39 richi Exp $
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


#if defined CPU_X86 && defined X86_3DNOW
#include "simd_x86_3dnow.h"
#elif defined CPU_X86 && defined X86_ISSE1
#include "simd_x86_isse1.h"
#endif


/* Define all undefined stuff with the default C code. */
#include "simd_c.h"


#endif
