/*
 * midi_io.c
 *
 * Copyright (C) 2000 Jim Garrison
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
#include "config.h"
#endif

#include "filter.h"
#include "midi.h"

#ifdef HAVE_ALSA

#include <sys/asoundlib.h>

int alsa_midi_out_f (filter_t *n)
{
#if 0
	filter_buffer_t *buf;
	filter_pipe_t *in;
	midi_event_t *mev;
	snd_seq_t *seq_handle = NULL;
	snd_seq_event_t sev;
	int cnt;

	if    (/*!(subs.sender.client = filterparam_val_int(filternode_get_param(n, "client")))
	    || !(subs.sender.port   = filterparam_val_int(filternode_get_param(n, "port")))
	    || !(subs.dest.client   = filterparam_val_int(filternode_get_param(n, "dest_client")))
	    || !(subs.dest.port     = filterparam_val_int(filternode_get_param(n, "dest_port")))
	    || */!(in = filternode_get_input(n, PORTNAME_IN)))
		return -1;

	if (snd_seq_open(&seq_handle, SND_SEQ_OPEN) < 0)
		return -1;
	snd_seq_set_client_name(seq_handle, "GLAME");
	snd_seq_set_client_group(seq_handle, "input");

	FILTER_AFTER_INIT;

	while ((buf = mbuf_get(in))) {
		FILTER_CHECK_STOP;
		cnt = mbuf_size(buf);
		mev = mbuf_buf(buf);
		while (cnt--) {
			1;
			mev++;
		}
		mbuf_unref(buf);
	}

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;
#endif

	return 0;
}

#endif

extern int midi_io_register()
{
	filter_t *f;

#ifdef HAVE_ALSA
	if (!(f = filter_creat(NULL))
	    || !filter_add_input(f, PORTNAME_IN, "input", FILTER_PORTTYPE_MIDI)
	    || !filter_add_param(f, "client", "alsa client number", FILTER_PARAMTYPE_INT)
	    || !filter_add_param(f, "port", "alsa port number", FILTER_PARAMTYPE_INT)
	    || !filter_add_param(f, "dest_client", "remote client number", FILTER_PARAMTYPE_INT)
	    || !filter_add_param(f, "dest_port", "remote port number", FILTER_PARAMTYPE_INT)
	    || filter_add(f, "midi_out", "alsa midi output"))
		return -1;
	f->f = alsa_midi_out_f;
#endif

	return 0;
}
