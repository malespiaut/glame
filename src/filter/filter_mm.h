#ifndef _FILTER_MM_H
#define _FILTER_MM_H

/*
 * filter_mm.h
 * $Id: filter_mm.h,v 1.8 2000/10/28 13:45:48 richi Exp $
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


filter_t *_filter_alloc(int type);
void _filter_free(filter_t *node);
filter_t *_filter_instantiate(filter_t *f);
// FIXME: add int _filter_apply(f, (int)(*)(filter_t *));

filter_launchcontext_t *_launchcontext_alloc();
void _launchcontext_free(filter_launchcontext_t *c);


filter_pipe_t *_pipe_alloc(filter_port_t *source, filter_port_t *dest);
void _pipe_free(filter_pipe_t *p);


#endif
