/*
 * audio_io_jack.c
 * $Id: audio_io_jack.c,v 1.1 2003/04/27 21:10:26 richi Exp $
 *
 * Copyright (C) 2003 Richard Guenther
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

#include <jack/jack.h>
#include "audio_io.h"

PLUGIN_SET(audio_io_jack, "jack_audio_out")
#if 0
PLUGIN_SET(audio_io_jack, "jack_audio_in jack_audio_out")
#endif


struct jack_priv {
	jack_client_t *client;
	jack_port_t **ports;
};

static int jack_out_process(jack_nframes_t nframes, filter_t *n)
{
	DPRINTF("ping\n");
	sbuf_unref(sbuf_get(n));
}

static void jack_shutdown(filter_launchcontext_t *c)
{
	DPRINTF("shutdown from jack\n");
	/* signal other threads, we're going to stop */
	atomic_inc(&c->result);
}

static int jack_out_launch_node(filter_t *n)
{
	struct jack_priv *p;
	int nports, i;

        if (!n || n->state == STATE_LAUNCHED)
                return 0;
        if (n->state != STATE_INITIALIZED)
                return -1;

        DPRINTF("initializing jack for %s\n", n->name);

	p = (struct jack_priv *)malloc(sizeof(struct jack_priv *));
	n->priv = p;

	/* We dont need to increment/decrement launch_context->val here.
	 * Just start processing and hope, jack copes with the delay... */

	/* register client to jack */
	if (!(p->client = jack_client_new("glame_out"))) {
		DPRINTF("jack_client_new failed\n");
		return -1;
	}
	jack_set_process_callback (p->client, jack_out_process, n);
	jack_on_shutdown (p->client, jack_shutdown, n->net->launch_context);
	if (jack_activate(p->client)) {
		DPRINTF("cannot activate client\n");
		return -1;
	}

	/* register and connect ports */
	nports = filterport_nrpipes(filterportdb_get_port(filter_portdb(n), PORTNAME_IN));
	p->ports = (jack_port_t **)malloc(sizeof(jack_port_t *) * nports);
	for (i = 0; i<nports; i++)
	{
		char name[64];
		snprintf("glame_jack_out%i", i);
		p->ports[i] = jack_port_register(p->client, name, JACK_DEFAULT_AUDIO_TYPE,
						 JackPortIsOutput, 0);
		if (!p->ports[i]) {
			DPRINTF("Cannot register %s\n", name);
			goto _out;
		}
	}
	for (i = 0; i<nports; i++)
	{
		char name[64];
		snprintf("%s%i", filterparam_val_string(filterparamdb_get_param(filter_paramdb(n), "device")));
		if (jack_connect(p->client, jack_port_name(p->ports[i]), name)) {
			DPRINTF("Cannot connect to %s\n", name);
			goto _out;
		}
	}


        n->net->launch_context->nr_threads++;
        n->state = STATE_LAUNCHED;

	return 0;
_out:
	jack_client_close(p->client);
	return -1;
}
static int jack_wait_node(filter_t *n)
{
	return 0;
}
static void jack_postprocess_node(filter_t *n)
{
	/* Stop processing, disconnect, deregister. */
	jack_client_close(p->client);
}

struct filter_operations jack_out_filter_ops;

int jack_audio_out_register(plugin_t *p)
{
	int res;
	res = aio_generic_register_output(p, "oss-audio-out",
					  jack_audio_out_f, "/dev/dsp");

	/* fixup ops */
	filter_t *f = plugin_filter(p);
	jack_out_filter_ops.init = f->ops->init;
	jack_out_filter_ops.launch = jack_out_launch_node;
	jack_out_filter_ops.launch = jack_wait_node;
	jack_out_filter_ops.launch = jack_postprocess_node;
	f->ops = &jack_out_filter_ops;
}

#if 0
int jack_audio_in_register(plugin_t *p)
{
	int res;
	res = aio_generic_register_input(p, "oss-audio-in",
	                                 jack_audio_in_f, "/dev/dsp");

	/* fixup ops */
	
}
#endif
