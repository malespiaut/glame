#ifndef _NETWORK_UTILS_H
#define _NETWORK_UTILS_H

/*
 * network_utils.h
 *
 * $Id: network_utils.h,v 1.1 2001/05/28 08:09:53 richi Exp $
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

#include "filter.h"
#include "gpsm.h"


/* Adds a swapfile_in node to the network streaming the gpsm node
 * swfile from start with size length (or full, if length -1).
 * Returns the added node or NULL on error. */
filter_t *net_add_gpsm_input(filter_t *net, gpsm_swfile_t *swfile,
			     long start, long length);

/* Adds a swapfile_out node to the network streaming into the gpsm node
 * swfile from start (truncated, if -1) with size length (or unlimited,
 * if length -1). Returns the added node or NULL on error. */
filter_t *net_add_gpsm_output(filter_t *net, gpsm_swfile_t *swfile,
			      long start, long length);

/* Applys the provided effect (by cloning it) to all unconnected
 * "out" ports in the network. Returns 0 on success, -1 on error. */
int net_apply_effect(filter_t *net, filter_t *effect);

/* Feeds all unconnected "out" ports into an audio_out filter doing
 * correct positioning/mixing, if necessary. Returns the audio out
 * filter on success, NULL on error. */
filter_t *net_apply_audio_out(filter_t *net);


#endif
