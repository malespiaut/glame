#ifndef _FILTER_PROTOCOLS_H
#define _FILTER_PROTOCOLS_H


/* Standard portnames.
 */
#define PORTNAME_IN "in"
#define PORTNAME_OUT "out"



/* Simply the SAMPLE protocol. What do you expect?
 */

typedef struct sbuf_header sbuf_header_t;
struct sbuf_header {
	char buf[1];
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


/*
 * How about some MIDI 
 */

typedef struct mbuf_header {
	char buf[1];
} mbuf_header_t;

#define mbuf_alloc(nrevents, filternode) \
        fbuf_alloc(sizeof(midi_event_t)*(nrevents) + sizeof(mbuf_header_t), \
		   &(filternode)->net->launch_context->buffers)
#define mbuf_size(fb) ((fb)==NULL?0:(fbuf_size(fb)-sizeof(mbuf_header_t))/sizeof(midi_event_t))
#define mbuf_buf(fb) ((midi_event_t *)(&((mbuf_header_t *)fbuf_buf(fb))->buf[0]))
#define mbuf_ref(fb) fbuf_ref(fb)
#define mbuf_unref(fb) fbuf_unref(fb)
#define mbuf_make_private(fb) fbuf_make_private(fb)
#define mbuf_get(p) fbuf_get(p)
#define mbuf_queue(p, fb) fbuf_queue(p, fb)

#endif
