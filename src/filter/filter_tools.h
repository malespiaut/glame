#ifndef _FILTER_TOOLS_H
#define _FILTER_TOOLS_H


/* Add your favorite generic tools for filter programming here.
 */


/* Support for feedback and generic fifo queues
 * inside a filter.
 */

typedef struct list_head feedback_fifo_t;
#define INIT_FEEDBACK_FIFO(fifo) INIT_LIST_HEAD(&(fifo))

struct fifo_entry {
	struct list_head list;
	filter_buffer_t *fb;
};

#define has_feedback(fifo) (!list_empty(fifo))

static inline void add_feedback(feedback_fifo_t *f, filter_buffer_t *fb)
{
	struct fifo_entry *e;

	e = (struct fifo_entry *)malloc(sizeof(struct fifo_entry));
	INIT_LIST_HEAD(&e->list);
	e->fb = fb;
	list_add_tail(&e->list, f);
}

static inline filter_buffer_t *get_feedback(feedback_fifo_t *f)
{
	struct fifo_entry *e;
	filter_buffer_t *fb;

	if (list_empty(f))
		return NULL;
	e = list_entry(f->next, struct fifo_entry, list);
	fb = e->fb;
	list_del(&e->list);
	free(e);

	return fb;
}


/* Generic structures and functions for n to one filters
 * without write buffering. Synchron operation.
 */
typedef struct {
	filter_pipe_t *in;
	filter_buffer_t *buf;
	SAMPLE *s;
	int pos;
} nto1_state_t;
static inline int nto1_init(nto1_state_t **I, filter_port_t *port)
{
    	int nr = filterport_nrpipes(port), i;
	filter_pipe_t *pipe;
	if (nr == 0)
	    	return -1;
	if (!(*I = ALLOCN(nr, nto1_state_t)))
		return -1;
	i = 0;
	filterport_foreach_pipe(port, pipe) {
	    	(*I)[i].in = pipe;
		(*I)[i].buf = NULL;
		(*I)[i].s = NULL;
		(*I)[i].pos = 0;
		i++;
	}
	return nr;
}
static inline void nto1_cleanup(nto1_state_t *I)
{
    	free(I);
}
static inline int nto1_head(nto1_state_t *I, int nr)
{
	int cnt, i;

	/* Find the maximum number of samples we can
	 * process in one run. But not more than
	 * GLAME_WBUFSIZE. */
	cnt = GLAME_WBUFSIZE;
	for (i=0; i<nr; i++) {
		if (!I[i].buf)
			continue;
		cnt = MIN(cnt, sbuf_size(I[i].buf) - I[i].pos);
	}
	/* Fix the resulting buffer positions. */
	for (i=0; i<nr; i++)
		if (I[i].buf)
			I[i].pos += cnt;
	return cnt;
}
static inline int nto1_tail(nto1_state_t *I, int nr)
{
	int nr_deactivated = 0, i;

	/* Check for missing buffers and EOFs.
	 * This does work as entry as well because
	 * sbuf_size(NULL) == 0 and I is prezeroed. */
	for (i=0; i<nr; i++) {
		if (!I[i].in)
			continue;
		if (sbuf_size(I[i].buf) == I[i].pos) {
			sbuf_unref(I[i].buf);
			I[i].buf = sbuf_get(I[i].in);
			I[i].s = sbuf_buf(I[i].buf);
			I[i].pos = 0;
			if (!I[i].buf) {
				I[i].in = NULL;
				nr_deactivated++;
			}
		}
	}
	return nr_deactivated;
}



/* SAMPLE to various type conversion including clipping of the
 * samples to [-1,1].
 */
static inline gl_s16 SAMPLE2SHORT(SAMPLE s)
{
        return (gl_s16)((s<-1.0 ? -1.0 : (s>1.0 ? 1.0 : s))
		*(s<0.0 ? (1<<15) : (1<<15)-1));
}
static inline gl_u16 SAMPLE2USHORT(SAMPLE s)
{	
	s += 1.0, s *= 0.5;
	return (gl_u16)((s<0.0 ? 0.0 : (s>1.0 ? 1.0 : s))*((1<<16)-1));
}
#define SHORT2SAMPLE(s)  ((SAMPLE)(gl_s16)(s)/(SAMPLE)(1<<15))
#define USHORT2SAMPLE(s) ((SAMPLE)(gl_u16)(s)/(SAMPLE)(1<<15) - 1.0)
static inline gl_s8 SAMPLE2CHAR(SAMPLE s)
{
        return (gl_s8)((s<-1.0 ? -1.0 : (s>1.0 ? 1.0 : s))
		*(s<0.0 ? (1<<7) : (1<<7)-1));
}
static inline gl_u8 SAMPLE2UCHAR(SAMPLE s)
{
	s += 1.0, s *= 0.5;
	return (gl_u8)((s<0.0 ? 0.0 : (s>1.0 ? 1.0 : s))*((1<<7)-1));
}
#define CHAR2SAMPLE(s)  ((SAMPLE)(gl_s8)(s)/(SAMPLE)(1<<7))
#define UCHAR2SAMPLE(s) ((SAMPLE)(gl_u8)(s)/(SAMPLE)(1<<7) - 1.0)

/* convert time in ms to number of samples */
#define TIME2CNT(type, time, rate) (type)(((time)*(rate))/1000.0)


/* Here follows a set of fast computing macros for standard operations.
 * To be implemented using ISSE/3DNOW stuff if available. Probably only
 * the higher count ones (SCALARPROD_XD_4).
 * Note that if called like SCALARPROD_1D_1(s, s, f) the compiler
 * can optimize away the destp++ test. Asm versions will want to do
 * seperate versions of both cases.
 */

#define SCALARPROD_1D_1(destp, source1p, fact1) \
do { \
	*destp = *(source1p++)*fact1; \
        if (&destp != &source1p) destp++; \
} while (0)

#define SCALARPROD_1D_4(destp, source1p, fact1) \
do { \
        SCALARPROD_1D_1(destp, source1p, fact1); \
        SCALARPROD_1D_1(destp, source1p, fact1); \
        SCALARPROD_1D_1(destp, source1p, fact1); \
        SCALARPROD_1D_1(destp, source1p, fact1); \
} while (0)

#define SCALARPROD_2D_1(destp, source1p, source2p, fact1, fact2) \
do { \
	*destp = *(source1p++)*fact1 + *(source2p++)*fact2; \
	if (&destp != &source1p) destp++; \
} while (0)
#define SCALARPROD_2D_4(destp, source1p, source2p, fact1, fact2) \
do { \
        SCALARPROD_2D_1(destp, source1p, source2p, fact1, fact2); \
        SCALARPROD_2D_1(destp, source1p, source2p, fact1, fact2); \
        SCALARPROD_2D_1(destp, source1p, source2p, fact1, fact2); \
        SCALARPROD_2D_1(destp, source1p, source2p, fact1, fact2); \
} while (0)

#define SCALARPROD_3D_1(destp, source1p, source2p, source3p, fact1, fact2, fact3) \
do { \
	*destp = *(source1p++)*fact1 + *(source2p++)*fact2 + *(source3p++)*fact3; \
	if (&destp != &source1p) destp++; \
} while (0)
#define SCALARPROD_3D_4(destp, source1p, source2p, source3p, fact1, fact2, fact3) \
do { \
        SCALARPROD_3D_1(destp, source1p, source2p, source3p, fact1, fact2, fact3); \
        SCALARPROD_3D_1(destp, source1p, source2p, source3p, fact1, fact2, fact3); \
        SCALARPROD_3D_1(destp, source1p, source2p, source3p, fact1, fact2, fact3); \
        SCALARPROD_3D_1(destp, source1p, source2p, source3p, fact1, fact2, fact3); \
} while (0)


#define INVERT1(destsourcep) \
do { \
	*destsourcep = -*destsourcep; \
	destsourcep++; \
} while (0)

#define INVERT4(destsourcep) \
do { \
	*destsourcep = -*destsourcep; \
	destsourcep++; \
	*destsourcep = -*destsourcep; \
	destsourcep++; \
	*destsourcep = -*destsourcep; \
	destsourcep++; \
	*destsourcep = -*destsourcep; \
	destsourcep++; \
} while (0)

#define ADD1(destsourcep,sum) \
do { \
	*destsourcep++ += sum; \
} while (0)

#define ADD4(destsourcep,sum) \
do { \
	*destsourcep++ += sum; \
	*destsourcep++ += sum; \
	*destsourcep++ += sum; \
	*destsourcep++ += sum; \
} while (0)

#endif
