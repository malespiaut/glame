#ifndef _FILTER_PROTOCOLS_H
#define _FILTER_PROTOCOLS_H


/* Standard portnames.
 */
#define PORTNAME_IN "in"
#define PORTNAME_OUT "out"
#define PORTNAME_LEFT_IN "left_in"
#define PORTNAME_RIGHT_IN "right_in"
#define PORTNAME_LEFT_OUT "left_out"
#define PORTNAME_RIGHT_OUT "right_out"



/* Simply the SAMPLE protocol. What do you expect?
 */

#define SBUF_POS_ERR    -1
#define SBUF_POS_LEFT    0
#define SBUF_POS_MONO    0	/* for your convenience :) */
#define SBUF_POS_RIGHT   1
#define SBUF_POS_ANY   128	/* in case you lose position information during mixing etc. just use this */

typedef struct sbuf_header sbuf_header_t;
struct sbuf_header {
	/* pos = left,right,
	 * any <-everything else is part of some 3D sound standard and 
	 * has to be handled by filters and user interaction
	 */
	int  pos;	
	char buf[0];
};
#define sbuf_alloc(nrsamples, filternode) \
        fbuf_alloc(SAMPLE_SIZE*(nrsamples) + sizeof(sbuf_header_t), \
		   &(filternode)->net->launch_context->buffers)
#define sbuf_size(fb) ((fb)==NULL?0:(fbuf_size(fb)-sizeof(sbuf_header_t))/SAMPLE_SIZE)
#define sbuf_buf(fb) ((SAMPLE *)(&((sbuf_header_t *)fbuf_buf(fb))->buf[0]))
#define sbuf_ref(fb) fbuf_ref(fb)
#define sbuf_unref(fb) fbuf_unref(fb)
#define sbuf_make_private(fb) fbuf_make_private(fb)
#define sbuf_get(p) fbuf_get(p)
#define sbuf_queue(p, fb) fbuf_queue(p, fb)
#define sbuf_pos(fb) (((sbuf_header_t *)fbuf_buf(fb))->pos)

/*
 * How 'bout some MIDI 
 */

typedef struct mbuf_header {
	char buf[0];
} mbuf_header_t;
#define mbuf_alloc(nrevents, filternode) \
        fbuf_alloc(sizeof(midi_event_t*(nrevents) + sizeof(mbuf_header_t), \
		   &(filternode)->net->launch_context->buffers)
#define mbuf_size(fb) ((fb)==NULL?0:(fbuf_size(fb)-sizeof(mbuf_header_t))/sizeof(midi_event_t)
#define mbuf_buf(fb) ((midi_event_t *)(&((mbuf_header_t *)fbuf_buf(fb))->buf[0]))
#define mbuf_ref(fb) fbuf_ref(fb)
#define mbuf_unref(fb) fbuf_unref(fb)
#define mbuf_make_private(fb) fbuf_make_private(fb)
#define mbuf_get(p) fbuf_get(p)
#define mbuf_queue(p, fb) fbuf_queue(p, fb)

#endif
