/*
 * file_io2.c
 * $Id: file_io2.c,v 1.1 2000/02/20 15:26:29 richi Exp $
 *
 * Copyright (C) 1999, 2000 Alexander Ehlert, Richard Günther
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
 *
 * Generic audiofile reader filter. Every generic reader should honour
 * the per-pipe parameter "position" by just selecting the "best matching"
 * channel of the file for the pipe. Remixing is not required, as is
 * duplicate channel output. The real position of the stream should be set
 * exact though.
 * Every generic reader should have a
 * - prepare method which does audiofile header reading and checking if
 *   it can handle the file. Fixup of the output pipes type is required, too.
 *   prepare is a unification of the connect_out & fixup_param method.
 * - f method which does the actual reading
 * - cleanup method to cleanup the private shared state
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "filter.h"
#include "util.h"

#ifdef HAVE_AUDIOFILE
#include <audiofile.h>
int audiofile_prepare(filter_node_t *n, const char *filename);
int audiofile_connect(filter_node_t *n, filter_pipe_t *p);
int audiofile_f(filter_node_t *n);
void audiofile_cleanup(filter_node_t *n);
#endif


typedef struct {
	struct list_head list;
	int (*prepare)(filter_node_t *, const char *);
	int (*connect)(filter_node_t *, filter_pipe_t *);
	int (*f)(filter_node_t *);
	void (*cleanup)(filter_node_t *);
} rw_t;

typedef struct {
	rw_t *rw;
	int initted;
	union {
		struct {
			int dummy;
		} dummy;
#ifdef HAVE_AUDIOFILE
		struct {
			int dummy;
			/* put your shared state stuff here */
		} audiofile;
#endif
	} u;
} rw_private_t;
#define RWPRIV(node) ((rw_private_t *)((node)->private))

/* the readers list */
static struct list_head readers;


static int add_reader(int (*prepare)(filter_node_t *, const char *),
		      int (*connect)(filter_node_t *, filter_pipe_t *),
		      int (*f)(filter_node_t *),
		      void (*cleanup)(filter_node_t *))
{
	rw_t *rw;

	if (!prepare || !f)
		return -1;
	if (!(rw = ALLOC(rw_t)))
		return -1;
	rw->prepare = prepare;
	rw->connect = connect;
	rw->f = f;
	rw->cleanup = cleanup;
	list_add(&rw->list, &readers);

	return 0;
}


static int read_file_init(filter_node_t *n)
{
	rw_private_t *p;

	if (!(p = ALLOC(rw_private_t)))
		return -1;
	n->private = p;
	return 0;
}
static void read_file_cleanup(filter_node_t *n)
{
	if (RWPRIV(n)->rw
	    && RWPRIV(n)->rw->cleanup)
		RWPRIV(n)->rw->cleanup(n);
	free(RWPRIV(n));
}
static int read_file_f(filter_node_t *n)
{
	/* require set filename (a selected reader) and
	 * at least one connected output. */
	if (!RWPRIV(n)->initted)
		return -1;
	if (!filternode_get_output(n, PORTNAME_OUT))
		return -1;
	return RWPRIV(n)->rw->f(n);
}
static int read_file_connect_out(filter_node_t *n, const char *port,
				 filter_pipe_t *p)
{
	/* no reader -> no filename -> some "defaults" */
	if (!RWPRIV(n)->rw) {
		filterpipe_settype_sample(p, 44100, FILTER_PIPEPOS_DEFAULT);
		return 0;
	}

	/* pass request to readers prepare, it can reject the
	 * connection, but not the file here. */
	return RWPRIV(n)->rw->connect(n, p);
}
static int read_file_fixup_param(filter_node_t *n, filter_pipe_t *p,
				 const char *name, filter_param_t *param)
{
	rw_t *r;

	/* only pipe param change (position)? */
	if (p && RWPRIV(n)->rw) {
		if (RWPRIV(n)->rw->connect(n, p) == -1)
			filternetwork_break_connection(p);
		return 0;
	
        /* filename change! */
	} else {
		/* check actual reader */
		if (RWPRIV(n)->rw)
			if (RWPRIV(n)->rw->prepare(n, filterparam_val_string(param)) != -1)
				goto reconnect;

		RWPRIV(n)->rw = NULL;
		RWPRIV(n)->initted = 0;

		/* search for applicable reader */
		list_foreach(&readers, rw_t, list, r) {
			if (r->prepare(n, name) != -1) {
				RWPRIV(n)->rw = r;
				goto reconnect;
			}
		}

		/* no reader found */
		return -1;
	}

 reconnect:
	/* re-connect all pipes */
	filternode_foreach_output(n, p)
		if (RWPRIV(n)->rw->connect(n, p) == -1)
			filternetwork_break_connection(p);

	return 0;
}


int file_io2_register()
{
	filter_t *f;
	filter_portdesc_t *p;

	INIT_LIST_HEAD(&readers);

	if (!(f = filter_alloc("read_file2", "Generic file read filter",
			       read_file_f))
	    || !(p = filter_add_output(f, PORTNAME_OUT, "output channels",
				       FILTER_PORTTYPE_SAMPLE|FILTER_PORTTYPE_AUTOMATIC))
	    || !filterport_add_param(p, "position", "position of the stream",
				     FILTER_PARAMTYPE_FLOAT)
	    || !filter_add_param(f, "filename", "filename",
				 FILTER_PARAMTYPE_STRING))
		return -1;
	f->init = read_file_init;
	f->cleanup = read_file_cleanup;
	f->connect_out = read_file_connect_out;
	f->fixup_param = read_file_fixup_param;
	if (filter_add(f) == -1)
		return -1;

#ifdef HAVE_AUDIOFILE
	add_reader(audiofile_prepare, audiofile_connect,
		   audiofile_f, audiofile_cleanup);
#endif

	return 0;
}


/* The actual readers.
 */

int audiofile_prepare(filter_node_t *n, const char *filename)
{
	return -1;
}
int audiofile_connect(filter_node_t *n, filter_pipe_t *p)
{
	return -1;
}
int audiofile_f(filter_node_t *n)
{
	return -1;
}
void audiofile_cleanup(filter_node_t *n)
{
}
