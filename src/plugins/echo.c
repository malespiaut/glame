/*
 * echo.c
 * $Id: echo.c,v 1.2 2000/03/15 16:29:32 richi Exp $
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
 * This is a sample implementation of an echo filter to show the right
 * use of feedback buffering.
 */

#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include "filter.h"
#include "util.h"


static int echo_f(filter_node_t *n)
{
	filter_param_t *param;
	int delay;  /* in samples */
	float mix, fbfact, infact;  /* mixing factor, 1/(1+mix) */
	filter_pipe_t *in, *out;
	feedback_fifo_t fifo;
	filter_buffer_t *inb, *fb;
	SAMPLE *ins, *fs;
	int cnt, inb_pos, fb_pos;

	if (!(in = filternode_get_input(n, PORTNAME_IN))
	    || !(out = filternode_get_output(n, PORTNAME_OUT)))
	        FILTER_ERROR_RETURN("no in- or output");

	delay = filterpipe_sample_rate(in)/10; /* 0.1 sec. default delay */
	if ((param = filternode_get_param(n, "time")))
		delay = (int)(filterpipe_sample_rate(in) 
				* filterparam_val_float(param)/1000.0);
	mix = 0.7;
	if ((param = filternode_get_param(n, "mix")))
		mix = filterparam_val_float(param);
	fbfact = mix/(1.0 + mix);
	infact = 1.0/(1.0 + mix);

	INIT_FEEDBACK_FIFO(fifo);


	FILTER_AFTER_INIT;

	/* First fill the feedback buffer with at least(!)
	 * delay samples and just forward all buffers completely
	 * in this delay. The goto is clever :) */
	cnt = 0;
	goto entry1;
	do {
	        FILTER_CHECK_STOP;
		sbuf_queue(out, inb); /* this really ate one reference */
	entry1:
		inb = sbuf_get(in);
		sbuf_ref(inb);
		add_feedback(&fifo, inb); /* this "ate" one reference! */
		cnt += sbuf_size(inb);
	} while (cnt <= delay);

	/* after this we should have one unsent buffer as inb which
	 * is queued already in the fifo and at least delay samples
	 * in the fifo. The position of the first to be processed
	 * sample in the current inb is cnt-delay.
	 */
	inb_pos = cnt - delay;

	/* While we get new input samples do the effect using the entry
	 * from the fifo as send buffer. The goto is clever again :)
	 */
	goto entry2;
	do {
	        FILTER_CHECK_STOP;
		/* check for feedback (and target) buffer overrun */
		if (fb_pos == sbuf_size(fb)) {
			/* one target is ready, we need to queue it
			 * _and_ put it back into the fifo, so we
			 * need another reference to it.
			 */
			sbuf_ref(fb);
			sbuf_queue(out, fb);
			add_feedback(&fifo, fb);

			/* Now get the next feedback/target buffer
			 * from the fifo. And prepare it for write
			 * access.
			 */
		entry2:
			fb = get_feedback(&fifo);
			fb = sbuf_make_private(fb);
			fb_pos = 0;
		}

		/* to be able to do a "fast loop" we check for the
		 * minimum available buffer size here.
		 */
		cnt = MIN(sbuf_size(fb) - fb_pos, sbuf_size(inb) - inb_pos);
		ins = sbuf_buf(inb) + inb_pos;
		fs = sbuf_buf(fb) + fb_pos;
		inb_pos += cnt;
		fb_pos += cnt;

		/* start "alignment" run */
		for (; (cnt & 3)>0; cnt--) {
		        SCALARPROD1_2(fs, ins, fbfact, infact);
		}
		/* do fast 4 products in one run loop */
		for (; cnt>0; cnt-=4) {
		        SCALARPROD4_2(fs, ins, fbfact, infact);
		}

		/* now we have to check which buffer has the underrun */
		if (inb_pos == sbuf_size(inb)) {
			/* In-buffer underrun is simple - we dont need
			 * the current one for anything anymore -> drop
			 * it. Then we just get a new one.
			 */
			sbuf_unref(inb);
			inb = sbuf_get(in);
			inb_pos = 0;
		}
		/* the check for the feedback buffer underrun is
		 * at the beginning of the loop so we can do the
		 * EOF at in-port check here.
		 */
	} while (inb);

	/* So, whats the state now? We have a partly finished target
	 * buffer and the fifo. So just queue the whole stuff and send
	 * an EOF afterwards.
	 * FIXME? Is this correct echo? I.e. the stream is extended by
	 * exactly delay samples, of course we could do an endless
	 * feedback with virtual zero input until the samples all settle
	 * to zero?? Hohumm...
	 */
	/* Remember: we dont need no extra references here as we let the
	 * buffers go and not queue them again in the fifo. The while
	 * condition at the end lets us automagically send the EOF mark.
	 */
	sbuf_queue(out, fb);
	do {
	        FILTER_CHECK_STOP;
		fb = get_feedback(&fifo);
		sbuf_queue(out, fb);
	} while (fb);

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	FILTER_RETURN;
}


/* Registry setup of all contained filters
 */
const char *echo_description = "The echo effect";
void *echo_pixmap = NULL;

int echo_register()
{
	filter_t *f;

	DPRINTF("echo_register()\n");

	if (!(f = filter_alloc("echo", "echo effect", echo_f))
	    || !filter_add_input(f, PORTNAME_IN, "input",
				FILTER_PORTTYPE_SAMPLE)
	    || !filter_add_output(f, PORTNAME_OUT, "output",
		    		FILTER_PORTTYPE_SAMPLE)
	    || !filter_add_param(f, "time", "echo time in ms",
		    		FILTER_PARAMTYPE_FLOAT)
	    || !filter_add_param(f, "mix", "mixer ratio",
		    		FILTER_PARAMTYPE_FLOAT))
		return -1;
	if (filter_add(f) == -1)
		return -1;

	return 0;
}
