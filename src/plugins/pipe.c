/*
 * pipe.c
 * $Id: pipe.c,v 1.11 2000/12/08 14:27:22 richi Exp $
 *
 * Copyright (C) 2000 Richard Guenther
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
 * This is a set of "pipe" filters.
 * - pipe-in feeds input from a pipe fed by a subprocess into the
 *   filternetwork
 * - pipe-out [not implemented]
 * - pipe-inout [not implemented]
 */

#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include "filter.h"
#include "util.h"
#include "glplugin.h"


PLUGIN_SET(pipe, "pipe_in pipe_out")


static int pipe_in_f(filter_t *n)
{
	filter_buffer_t *lbuf, *rbuf;
	filter_pipe_t *lout, *rout;
	SAMPLE *ls, *rs;
	short *b, *bb;
	FILE *p;
	char cmd[256], *s;
	int res, q;
	
	if (!(lout = filternode_get_output(n, PORTNAME_OUT))
	    || !(s = filterparam_val_string(filternode_get_param(n, "cmd"))))
		FILTER_ERROR_RETURN("insufficient configuration");
	rout = filternode_next_output(lout);

	q = 2;
	if (rout)
		q = 4;

	strncpy(cmd, s, 255);
	if ((s = filterparam_val_string(filternode_get_param(n, "tail"))))
		strncat(cmd, s, 255);

	if (!(p = popen(cmd, "r")))
		FILTER_ERROR_RETURN("popen failed");
	b = (short *)malloc(q*4096);

	FILTER_AFTER_INIT;

	while ((res = fread(b, q, 4096, p)) > 0) {
		FILTER_CHECK_STOP;

		lbuf = sbuf_make_private(sbuf_alloc(res, n));
		ls = sbuf_buf(lbuf);
		if (rout) {
			rbuf = sbuf_make_private(sbuf_alloc(res, n));
			rs = sbuf_buf(rbuf);
		}
		bb = b;
		do {
			*(ls++) = SHORT2SAMPLE(*(bb++));
			if (rout)
				*(rs++) = SHORT2SAMPLE(*(bb++));
			res--;
		} while (res > 0);

		sbuf_queue(lout, lbuf);
		if (rout)
			sbuf_queue(rout, rbuf);
	}
	sbuf_queue(lout, NULL);
	if (rout)
		sbuf_queue(rout, NULL);

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	pclose(p);
	free(b);

	FILTER_RETURN;
}

static int pipe_in_connect_out(filter_t *source, filter_port_t *port,
			       filter_pipe_t *p)
{
	int rate;

	if (filterport_nrpipes(port) > 1)
		return -1;

	rate = filterparam_val_int(filternode_get_param(source, "rate"));

	if (filterport_nrpipes(port) == 0) {
		filterpipe_settype_sample(p, rate, FILTER_PIPEPOS_DEFAULT);
	} else {
		filterpipe_settype_sample(p, rate, FILTER_PIPEPOS_RIGHT);
		p = filternode_get_output(source, PORTNAME_OUT);
		filterpipe_settype_sample(p, rate, FILTER_PIPEPOS_LEFT);
	}

	return 0;
}

static void pipe_in_param_changed(glsig_handler_t *h, long sig, va_list va)
{
    filter_param_t *param;
    filter_port_t *out;
    filter_pipe_t *outp;
    filter_t *filter;

    GLSIGH_GETARGS1(va, param);
    if (strcmp(filterparam_label(param), "rate") != 0)
	return;

    filter = filterparam_filter(param);
    out = filterportdb_get_port(filter_portdb(filter), PORTNAME_OUT);
    filterport_foreach_pipe(out, outp) {
	filterpipe_settype_sample(outp,
				  filterparam_val_int(param),
				  filterpipe_sample_hangle(outp));
	glsig_emit(&outp->emitter, GLSIG_PIPE_CHANGED, outp);
    }
}

int pipe_in_register(plugin_t *p)
{
	filter_t *f;

	if (!(f = filter_creat(NULL))
	    || !filter_add_output(f, PORTNAME_OUT, "output",
				  FILTER_PORTTYPE_SAMPLE))
		return -1;
	f->f = pipe_in_f;

	filterparamdb_add_param_string(filter_paramdb(f), "cmd", 
				   FILTER_PARAMTYPE_STRING, NULL,
				   FILTERPARAM_DESCRIPTION, "command string",
				   FILTERPARAM_END);
	filterparamdb_add_param_string(filter_paramdb(f), "tail",
				   FILTER_PARAMTYPE_FILENAME, NULL,
				   FILTERPARAM_DESCRIPTION, "command string tail",
				   FILTERPARAM_END);
	filterparamdb_add_param_int(filter_paramdb(f), "rate",
				FILTER_PARAMTYPE_INT, GLAME_DEFAULT_SAMPLERATE,
				FILTERPARAM_DESCRIPTION, "data samplerate",
				FILTERPARAM_END);

	f->connect_out = pipe_in_connect_out;
	glsig_add_handler(&f->emitter, GLSIG_PARAM_CHANGED,
			  pipe_in_param_changed, NULL);

	plugin_set(p, PLUGIN_DESCRIPTION, "pipe input");
	plugin_set(p, PLUGIN_CATEGORY, "InOut");
	filter_register(f, p);

	return 0;
}



static int pipe_out_f(filter_t *n)
{
	filter_port_t *port;
	nto1_state_t I[2] = { {NULL, NULL, NULL, 0 }, {NULL, NULL, NULL, 0 } };
	SAMPLE sample;
	short *b, *bb;
	FILE *p;
	char cmd[256], *s;
	int res = 1, q, nr, nr_active, cnt, ccnt, i;

	/* Get the pipes. */
	port = filterportdb_get_port(filter_portdb(n), PORTNAME_IN);
	if ((nr = filterport_nrpipes(port)) < 1)
		FILTER_ERROR_RETURN("insufficient inputs");
	I[0].in = filterport_get_pipe(port);
	I[1].in = filterport_next_pipe(port, I[0].in);
	q = 2;
	if (I[1].in)
		q = 4;

	/* Initialize the command string out of the cmd/tail parameters.
	 * Very simple and probably broken somehow. */
	if (!(s = filterparam_val_string(filterparamdb_get_param(filter_paramdb(n), "cmd"))))
		FILTER_ERROR_RETURN("no command");
	strncpy(cmd, s, 255);
	if ((s = filterparam_val_string(filternode_get_param(n, "tail")))) {
		char tail[256], *ratep;
		strncpy(tail, s, 255);
		ratep = strstr(tail, "%%r");
		if (ratep) {
			strncat(cmd, tail, ratep-tail);
			sprintf(cmd+strlen(cmd), "%i", filterpipe_sample_rate(I[0].in));
			strcat(cmd, ratep+3);
		} else
			strcat(cmd, tail);
	}
	if (!(p = popen(cmd, "w")))
		FILTER_ERROR_RETURN("popen failed");

	b = malloc(q*GLAME_WBUFSIZE);

	FILTER_AFTER_INIT;

	nr_active = nr;
	goto entry;
	do {
		FILTER_CHECK_STOP;

		/* Head. Find maximum number of processable samples,
		 * fix destination position. */
		cnt = nto1_head(I, nr);

		/* Allocate intermediate output buffer and copy/convert
		 * the input. */
		bb = b;
		ccnt = cnt;
		while (ccnt--) {
			for (i=0; i<nr; i++) {
				if (!I[i].buf)
					sample = 0.0;
				else
					sample = *I[i].s++;
				*bb++ = SAMPLE2SHORT(sample);
			}
		}

		/* Write the buffer to the programs input pipe. */
		res = fwrite(b, q, cnt, p);

	entry:
		/* Tail & entry. Check if we need to get additional
                 * buffers, recognize EOF's. */
                nr_active -= nto1_tail(I, nr);
	} while (nr_active>0 && res>0);

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	pclose(p);
	free(b);

	FILTER_RETURN;
}

static int pipe_out_connect_in(filter_t *source, filter_port_t *port,
			       filter_pipe_t *p)
{
        filter_pipe_t *pipe;

	if (filterport_nrpipes(port) > 1)
		return -1;

	if (!(pipe = filterport_get_pipe(port)))
	        return 0;
	if (filterpipe_sample_rate(pipe) != filterpipe_sample_rate(p))
	        return -1;

	return 0;
}

int pipe_out_register(plugin_t *p)
{
	filter_t *f;

	if (!(f = filter_creat(NULL))
	    || !filter_add_input(f, PORTNAME_IN, "input",
				 FILTER_PORTTYPE_SAMPLE))
		return -1;
	f->f = pipe_out_f;

	filterparamdb_add_param_string(filter_paramdb(f), "cmd", 
				   FILTER_PARAMTYPE_STRING, NULL,
				   FILTERPARAM_DESCRIPTION, "command string",
				   FILTERPARAM_END);
	filterparamdb_add_param_string(filter_paramdb(f), "tail",
				       FILTER_PARAMTYPE_FILENAME, NULL,
				       FILTERPARAM_DESCRIPTION,
				       "command string tail\n"
				       "use %%r for sample rate",
				       FILTERPARAM_END);

	f->connect_in = pipe_out_connect_in;

	plugin_set(p, PLUGIN_DESCRIPTION, "pipe output");
	plugin_set(p, PLUGIN_CATEGORY, "InOut");
	filter_register(f, p);

	return 0;
}
