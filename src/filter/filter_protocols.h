#ifndef _FILTER_PROTOCOLS_H
#define _FILTER_PROTOCOLS_H

/* Standard portnames.
 */
#define PORTNAME_IN "in"
#define PORTNAME_OUT "out"
#define PORTNAME_LEFT_IN "left_in"
#define PORTNAME_RIGHT_IN "right_in"



/* The SAMPLE protocol. The pos field of the header is currently unused
 */

struct sbuf_header {
	int pos;
	char buf[0];
};
#define sbuf_alloc(nrsamples, filternode) \
        fbuf_alloc(SAMPLE_SIZE*(nrsamples) + sizeof(struct sbuf_header), \
		   &(filternode)->launch_context->buffers)
#define sbuf_size(fb) ((fb)==NULL?0:(fbuf_size(fb)-sizeof(struct sbuf_header))/SAMPLE_SIZE)
#define sbuf_buf(fb) ((SAMPLE *)(&((struct sbuf_header *)fbuf_buf(fb))->buf[0]))
#define sbuf_ref(fb) fbuf_ref(fb)
#define sbuf_unref(fb) fbuf_unref(fb)
#define sbuf_make_private(fb) fbuf_make_private(fb)
#define sbuf_get(p) fbuf_get(p)
#define sbuf_queue(p, fb) fbuf_queue(p, fb)


#endif
