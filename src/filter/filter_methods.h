#ifndef _FILTER_METHODS_H
#define _FILTER_METHODS_H

/*
 * filter_methods.h
 * $Id: filter_methods.h,v 1.8 2001/04/11 08:39:02 richi Exp $
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


/* default filter methods.
 */
int filter_default_connect_out(filter_t *n, filter_port_t *port,
			       filter_pipe_t *p);
int filter_default_connect_in(filter_t *n, filter_port_t *port,
			      filter_pipe_t *p);
int filter_default_set_param(filter_t *n, filter_param_t *param,
			     const void *val);


/* network filter methods.
 */
int filter_network_init(filter_t *n);
int filter_network_f(filter_t *n);
int filter_network_connect_out(filter_t *source, filter_port_t *port,
			       filter_pipe_t *p);
int filter_network_connect_in(filter_t *dest, filter_port_t *port,
			      filter_pipe_t *p);
int filter_network_set_param(filter_t *n, filter_param_t *param,
			     const void *val);


#endif
