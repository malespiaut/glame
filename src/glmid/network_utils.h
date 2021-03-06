#ifndef _NETWORK_UTILS_H
#define _NETWORK_UTILS_H

/*
 * network_utils.h
 *
 * $Id: network_utils.h,v 1.10 2001/12/09 16:09:56 richi Exp $
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


/* Adds an instance of the specified plugin (by name) to the network.
 * Returns the created instance or NULL on error. */
filter_t *net_add_plugin_by_name(filter_t *net, const char *plugin);

/* Adds a swapfile_in node to the network streaming the gpsm node
 * swfile from start with size length (or full, if length -1).
 * Returns the added node or NULL on error. */
filter_t *net_add_gpsm_input(filter_t *net, gpsm_swfile_t *swfile,
			     long start, long length, long flags);

/* Adds a swapfile_out node to the network streaming into the gpsm node
 * swfile from start (truncated, if -1) with size length (or unlimited,
 * if length -1). Returns the added node or NULL on error. */
filter_t *net_add_gpsm_output(filter_t *net, gpsm_swfile_t *swfile,
			      long start, long length, long flags);

/* Applys the provided effect (by cloning it) to all unconnected
 * matching output ports in the network. The first available input port
 * of the effect is used. Returns 0 on success, -1 on error. */
int net_apply_effect(filter_t *net, filter_t *effect);

/* Applys the provided node (already inside the net) to all unconnected
 * matching output ports in the network. The first available input port
 * of the node is used. Returns 0 on success, -1 on error. */
int net_apply_node(filter_t *net, filter_t *node);

/* Feeds all unconnected output ports into an audio_out filter doing
 * correct positioning/mixing, if necessary. Returns the audio out
 * filter on success, NULL on error. */
filter_t *net_apply_audio_out(filter_t *net);


/* Links params of two filters, i.e. updates of source will update
 * dest, too (but not the other way around). */
void *net_link_params(filter_t *dest, filter_t *source);
void net_unlink_params(void *handle);


#endif
