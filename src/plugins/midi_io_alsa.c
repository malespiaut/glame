/*
 * audio_io_midi.c
 * $Id: midi_io_alsa.c,v 1.3 2003/04/11 20:10:36 richi Exp $
 *
 * Copyright (C) 2002 Alexander Ehlert
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

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "filter.h"
#include <math.h>

#ifdef ALSA_H_IN_SYS
#include <sys/asoundlib.h>
#else
#include <alsa/asoundlib.h>
#endif

static snd_seq_t* alsa_seq_handle=NULL;
static float midifreq[128];

snd_seq_t *open_alsa_sequencer() {

  snd_seq_t *seq_handle;
  int portid;

  if (snd_seq_open(&seq_handle, "default", SND_SEQ_OPEN_INPUT, 0) < 0) {
    DPRINTF("Error opening ALSA sequencer.\n");
    return NULL;
  }

  snd_seq_set_client_name(seq_handle, "GLAME Midi Input");
  if ((portid = snd_seq_create_simple_port(seq_handle, "GLAME Midi Input",
            SND_SEQ_PORT_CAP_WRITE|SND_SEQ_PORT_CAP_SUBS_WRITE,
            SND_SEQ_PORT_TYPE_APPLICATION)) < 0) {
    DPRINTF("Error creating sequencer port.\n");
    return NULL;
  }
  
  return(seq_handle);
}

static int midi_in_alsa_f(filter_t * n)
{
	filter_port_t *in, *trigger, *velocity, *freq;
	filter_pipe_t *inp, *tpipe, *vpipe, *fpipe;
	int npfd;
	struct pollfd *pfd;
	filter_buffer_t *tb,*vb,*fb, *b;
	int cnt;
	SAMPLE *tbuf, *vbuf, *fbuf, t, v, f;
	snd_seq_event_t *ev;
	int i,events,pcnt;

	npfd = snd_seq_poll_descriptors_count(alsa_seq_handle, POLLIN);
	pfd = (struct pollfd *)alloca(npfd * sizeof(struct pollfd));
	snd_seq_poll_descriptors(alsa_seq_handle, pfd, npfd, POLLIN);

	DPRINTF("Have %d poll descriptors\n", npfd);

	in = filterportdb_get_port(filter_portdb(n), PORTNAME_IN);
	inp = filterport_get_pipe(in);

	if(!inp)
		FILTER_ERROR_RETURN("need sync stream\n");
	
       	trigger = filterportdb_get_port(filter_portdb(n), "trigger");
	tpipe = filterport_get_pipe(trigger);

	if(tpipe) {
		DPRINTF("Something wants a trigger\n");
	}

	freq = filterportdb_get_port(filter_portdb(n), "frequency");
	fpipe = filterport_get_pipe(freq);

	if(fpipe)
		DPRINTF("Something wants a frequency\n");	

	velocity = filterportdb_get_port(filter_portdb(n), "velocity");
	vpipe = filterport_get_pipe(velocity);
	if(vpipe)
		DPRINTF("Something wants velocity\n");

	if ( !tpipe && !vpipe && !fpipe)
		FILTER_ERROR_RETURN("you didn't connect anything to the output, so what do you expect?");
	
	FILTER_AFTER_INIT;
	
	t=v=0.0f;
	events=0;

	while ((b = sbuf_get(inp))) {
		FILTER_CHECK_STOP;

		if (tpipe) {
			sbuf_ref(b);
			tb = sbuf_make_private(b);
			tbuf = sbuf_buf(tb);
		}

		if (vpipe) {
			sbuf_ref(b);
			vb = sbuf_make_private(b);
			vbuf = sbuf_buf(vb);
		}	

		if (fpipe) {
			sbuf_ref(b);
			fb = sbuf_make_private(b);
			fbuf = sbuf_buf(fb);
		}

		cnt = sbuf_size(b);	     
		sbuf_unref(b);

		if (poll(pfd, npfd, 0) > 0) {
			DPRINTF("Uuuh, shalala, there's a midi event waiting for me(%d)\n", events++);
			pcnt = snd_seq_event_input_pending(alsa_seq_handle, 0);
			DPRINTF("%d events pending\n", pcnt);
			//while (snd_seq_event_input_pending(alsa_seq_handle, 0) > 0) {
				DPRINTF("Got midi event\n");
				snd_seq_event_input(alsa_seq_handle, &ev);
				switch (ev->type) {
				case SND_SEQ_EVENT_CONTROLLER: 
					DPRINTF("Control event on Channel %2d: %5d       \n",
						ev->data.control.channel, ev->data.control.value);
					break;
				case SND_SEQ_EVENT_PITCHBEND:
					DPRINTF("Pitchbender event on Channel %2d: %5d   \n", 
						ev->data.control.channel, ev->data.control.value);
					break;
				case SND_SEQ_EVENT_NOTEON:
					DPRINTF("Note On event on Channel %2d: %5d       \n",
						ev->data.control.channel, ev->data.note.note);
					t=1.0f;
					v=(SAMPLE)ev->data.note.velocity;
					f=midifreq[ev->data.note.note];
					break;        
				case SND_SEQ_EVENT_NOTEOFF: 
					DPRINTF("Note Off event on Channel %2d: %5d      \n",         
						ev->data.control.channel, ev->data.note.note);
					t=0.0f;
					break;
				}
				snd_seq_free_event(ev);
				DPRINTF("t=%f, v=%f f=%f\n",t,v,f);
				//}
		}

		if (tpipe) {
			for(i=0;i<cnt;i++)
				tbuf[i]=t;
			sbuf_queue(tpipe, tb);
		}

		if(vpipe) {
			for(i=0;i<cnt;i++)
				vbuf[i]=v;

			sbuf_queue(vpipe, vb);
		}

		if(fpipe) {
			for(i=0;i<cnt;i++)
				fbuf[i]=f;

			sbuf_queue(fpipe, fb);
		}	
			
	}

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	FILTER_RETURN;
}

int midi_in_alsa_register(plugin_t *p)
{
	filter_t *f;
	filter_param_t *param;
	filter_port_t *port;
	int x;
	float a = 440.0f; // a is 440 hz...

	for (x = 0; x < 128; ++x)
		midifreq[x] = (a / 32) * pow(2.0f, ((float)x - 9.0f) / 12.0f);	

	alsa_seq_handle =  open_alsa_sequencer();

	if (alsa_seq_handle==NULL) {
		DPRINTF("Couldn't open alsa midi sequencer\n");
		return -1;
	}

	if (!(f = filter_creat(NULL)))
		return -1;

	f->f = midi_in_alsa_f;

	port = filterportdb_add_port(
		filter_portdb(f), PORTNAME_IN,
		FILTER_PORTTYPE_SAMPLE,
		FILTER_PORTFLAG_INPUT,
		FILTERPORT_DESCRIPTION, "input stream to sync on",
		FILTERPORT_END);

	port = filterportdb_add_port(
		filter_portdb(f), "trigger",
		FILTER_PORTTYPE_SAMPLE,
		FILTER_PORTFLAG_OUTPUT,
		FILTERPORT_DESCRIPTION, "trigger stream 0.0 is NoteOFF, 1.0 is NoteON",
		FILTERPORT_END);

	port = filterportdb_add_port(
		filter_portdb(f), "velocity",
		FILTER_PORTTYPE_SAMPLE,
		FILTER_PORTFLAG_OUTPUT,
		FILTERPORT_DESCRIPTION, "velocity of current triggered note",
		FILTERPORT_END);

	port = filterportdb_add_port(
		filter_portdb(f), "frequency",
		FILTER_PORTTYPE_SAMPLE,
		FILTER_PORTFLAG_OUTPUT,
		FILTERPORT_DESCRIPTION, "frequency of current note",
		FILTERPORT_END);

	plugin_set(p, PLUGIN_DESCRIPTION,
		   "convert midi input to stream");
	plugin_set(p, PLUGIN_PIXMAP, "midi_in.png");
	plugin_set(p, PLUGIN_CATEGORY, "Midi");
	plugin_set(p, PLUGIN_GUI_HELP_PATH, "Midi");
  
	return filter_register(f, p);
}
