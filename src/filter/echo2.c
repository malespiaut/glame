/*
 * echo2.c
 * $Id: echo2.c,v 1.4 2000/02/14 00:51:26 mag Exp $
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


static int echo2_f(filter_node_t *n)
{
	filter_param_t *param;
	int delay;  /* in samples */
	float mix, rdiv;  /* mixing factor, 1/(1+mix) */
	filter_pipe_t *in, *out;
	feedback_fifo_t fifo;
	filter_buffer_t *inb, *fb;
	SAMPLE *ins, *fs;
	int cnt, inb_pos, fb_pos;

	if (!(in = hash_find_input(PORTNAME_IN, n))
	    || !(out = hash_find_output(PORTNAME_OUT, n)))
	{
		FILTER_AFTER_INIT;
		goto _cleanup;
	}

	delay = in->u.sample.rate; /* 1 sec. default delay */
	if ((param = hash_find_param("time", n)))
		delay = (int)(in->u.sample.rate*param->val.f);
	mix = 0.7;
	if ((param = hash_find_param("mix", n)))
		mix = param->val.f;
	rdiv = 1.0/(1.0 + mix);

	INIT_FEEDBACK_FIFO(fifo);


	FILTER_AFTER_INIT;

	/* First fill the feedback buffer with at least(!)
	 * delay samples and just forward all buffers completely
	 * in this delay. The goto is clever :) */
	cnt = 0;
	goto entry1;
	do {
		/* we need to get a reference for the queued buffer */
		sbuf_ref(inb);
		sbuf_queue(out, inb);
	entry1:
		inb = sbuf_get(in);
		add_feedback(&fifo, inb); /* this ate our reference! */
		cnt += sbuf_size(inb);
	} while (pthread_testcancel(), cnt <= delay);

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
			if (!fb) DPRINTF("Feedback FIFO empty!\n");
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
		DPRINTF("cnt=%d",cnt);
		for (; cnt>0; cnt--) {
			*fs = ((*fs)*mix + *ins)*rdiv;
			fs++;
			ins++;
		}
		DPRINTF(" loop done.\n");
		/* now we have to check which buffer has the underrun */
		if (inb_pos == sbuf_size(inb)) {
			/* In-buffer underrun is simple - we dont need
			 * the current one for anything anymore -> drop
			 * it. Then we just get a new one.
			 */
			DPRINTF("sbuf_unref");
			sbuf_unref(inb);
			inb = sbuf_get(in);
			inb_pos = 0;
			DPRINTF(" done\n");
		}
		DPRINTF("pthread_testcancel()\n");
		/* the check for the feedback buffer underrun is
		 * at the beginning of the loop so we can do the
		 * EOF at in-port check here.
		 */
	} while (pthread_testcancel(), inb);

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
		fb = get_feedback(&fifo);
		sbuf_queue(out, fb);
	} while (fb);

_cleanup:
	FILTER_BEFORE_CLEANUP;

	return 0;
}

/* Registry setup of all contained filters
 */

void echo2_fixup_break_in(filter_node_t *n,filter_pipe_t *in)
{
	filter_pipe_t *out;
	out=hash_find_output(PORTNAME_OUT,n);
	if(out) filternetwork_break_connection(out);
}

int echo2_register()
{
	filter_t *f;

	if (!(f = filter_alloc("echo2", "echo effect", echo2_f))
	    || !filter_add_input(f, PORTNAME_IN, "input",
				FILTER_PORTTYPE_SAMPLE)
	    || !filter_add_output(f, PORTNAME_OUT, "output",
		    		FILTER_PORTTYPE_SAMPLE)
	    || !filter_add_param(f, "time", "echo time in s",
		    		FILTER_PARAMTYPE_FLOAT)
	    || !filter_add_param(f, "mix", "mixer ratio",
		    		FILTER_PARAMTYPE_FLOAT))
		return -1;
	f->fixup_break_in = echo2_fixup_break_in;
	
	if (filter_add(f) == -1)
		return -1;

	return 0;
}
