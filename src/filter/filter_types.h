#ifndef _FILTER_TYPES_H
#define _FILTER_TYPES_H

/*
 * filter_types.h
 * $Id: filter_types.h,v 1.2 2001/04/11 08:39:02 richi Exp $
 *
 * Copyright (C) 1999, 2000, 2001 Richard Guenther
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


/* Types that are publically visible - see seperate
 * headerfile for their definitions: filter.h,
 * filter_pipe.h, filter_port.h
 */
struct filter;
typedef struct filter filter_t;

struct filter_pipe;
typedef struct filter_pipe filter_pipe_t;

struct filter_portdb;
typedef struct filter_portdb filter_portdb_t;
struct filter_port;
typedef struct filter_port filter_port_t;

struct filter_buffer;
typedef struct filter_buffer filter_buffer_t;


/* Opaque types used internally only in filter_ops.h
 */
struct filter_operations;

struct filter_launchcontext;
typedef struct filter_launchcontext filter_launchcontext_t;


#endif
