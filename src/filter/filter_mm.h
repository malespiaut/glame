#ifndef _FILTER_MM_H
#define _FILTER_MM_H

/*
 * filter_mm.h
 * $Id: filter_mm.h,v 1.7 2000/05/01 12:22:16 richi Exp $
 *
 * Copyright (C) 1999, 2000 Richard Guenther
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

filter_t *_filter_alloc(int flags);
void _filter_free(filter_t *f);

filter_launchcontext_t *_launchcontext_alloc();
void _launchcontext_free(filter_launchcontext_t *c);


filter_pipe_t *_pipe_alloc(filter_portdesc_t *source, filter_portdesc_t *dest);
void _pipe_free(filter_pipe_t *p);

filter_node_t *_filter_instantiate(filter_t *f, const char *name);
void _node_free(filter_node_t *n);
void _network_free(filter_network_t *net);


/* future obsolete stuff */
filter_portdesc_t *_portdesc_alloc(filter_t *filter, const char *label,
				   int type, const char *desc);
void _portdesc_free(filter_portdesc_t *d);


#endif







