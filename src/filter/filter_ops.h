#ifndef _FILTER_OPS_H
#define _FILTER_OPS_H

/*
 * filter_ops.h
 * $Id: filter_ops.h,v 1.5 2001/04/11 08:39:02 richi Exp $
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

#include "filter_types.h"


extern struct filter_operations filter_node_ops;
extern struct filter_operations filter_network_ops;

struct filter_launchcontext {
	int nr_threads;
	pthread_t waiter;

	int state;

	int semid;
	glame_atomic_t result;
};

#endif
