#ifndef _FILTER_TOOLS_H
#define _FILTER_TOOLS_H


/* add your favorite generic tools for filter programming here */
typedef struct list_head feedback_fifo_t;
#define INIT_FEEDBACK_FIFO(fifo) INIT_LIST_HEAD(&(fifo))

struct fifo_entry {
	struct list_head list;
	filter_buffer_t *fb;
};

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


/* SAMPLE to various type conversion including clipping of the
 * samples to [-1,1].
 */
static inline short SAMPLE2SHORT(SAMPLE s)
{
        return (short)((s<-1.0 ? -1.0 : (s>1.0 ? 1.0 : s))*(1<<15));
}
static inline unsigned short SAMPLE2USHORT(SAMPLE s)
{	
	s += 1.0;
	return (unsigned short)((s<0.0 ? 0.0 : (s>2.0 ? 2.0 : s))*(1<<15));
}
#define SHORT2SAMPLE(s) ((SAMPLE)s/(SAMPLE)(1<<15))
static inline signed char SAMPLE2CHAR(SAMPLE s)
{
        return (char)((s<-1.0 ? -1.0 : (s>1.0 ? 1.0 : s))*(1<<7));
}
static inline unsigned char SAMPLE2UCHAR(SAMPLE s)
{
	s += 1.0;
	return (unsigned char)((s<0.0 ? 0.0 : (s>2.0 ? 2.0 : s))*(1<<7));
}
#define CHAR2SAMPLE(s)  ((SAMPLE)s/(SAMPLE)(1<<7))

/* convert time in ms to number of samples */
#define TIME2CNT(type, time, rate) (type)(time*rate/1000.0)

/* Here follows a set of fast computing macros for standard operations.
 * To be implemented using ISSE/3DNOW stuff if available.
 */

#define SCALARPROD1_1(destsourcep, fact) \
do { \
	*destsourcep = (*destsourcep)*fact; \
        if (*destsourcep<-1.0) *destsourcep = -1.0; \
        else if (*destsourcep>1.0) *destsourcep = 1.0; \
        destsourcep++; \
} while (0)

#define SCALARPROD4_1(destsourcep, fact) \
do { \
	*destsourcep = (*destsourcep)*fact; \
        if (*destsourcep<-1.0) *destsourcep = -1.0; \
        else if (*destsourcep>1.0) *destsourcep = 1.0; \
        destsourcep++; \
	*destsourcep = (*destsourcep)*fact; \
        if (*destsourcep<-1.0) *destsourcep = -1.0; \
        else if (*destsourcep>1.0) *destsourcep = 1.0; \
        destsourcep++; \
	*destsourcep = (*destsourcep)*fact; \
        if (*destsourcep<-1.0) *destsourcep = -1.0; \
        else if (*destsourcep>1.0) *destsourcep = 1.0; \
        destsourcep++; \
	*destsourcep = (*destsourcep)*fact; \
        if (*destsourcep<-1.0) *destsourcep = -1.0; \
        else if (*destsourcep>1.0) *destsourcep = 1.0; \
        destsourcep++; \
} while (0)


#define SCALARPROD1_2(destsource1p, source2p, fact1, fact2) \
do { \
	*destsource1p = (*destsource1p)*fact1 + (*source2p)*fact2; \
	destsource1p++; source2p++; \
} while (0)
#define SCALARPROD4_2(destsource1p, source2p, fact1, fact2) \
do { \
	*destsource1p = (*destsource1p)*fact1 + (*source2p)*fact2; \
	destsource1p++; source2p++; \
	*destsource1p = (*destsource1p)*fact1 + (*source2p)*fact2; \
	destsource1p++; source2p++; \
	*destsource1p = (*destsource1p)*fact1 + (*source2p)*fact2; \
	destsource1p++; source2p++; \
	*destsource1p = (*destsource1p)*fact1 + (*source2p)*fact2; \
	destsource1p++; source2p++; \
} while (0)

#define SCALARPROD1_2_d(destp, source1p, source2p, fact1, fact2) \
do { \
	*destp = (*source1p)*fact1 + (*source2p)*fact2; \
	destp++; source1p++; source2p++; \
} while (0)
#define SCALARPROD4_2_d(destp, source1p, source2p, fact1, fact2) \
do { \
	*destp = (*source1p)*fact1 + (*source2p)*fact2; \
	destp++; source1p++; source2p++; \
	*destp = (*source1p)*fact1 + (*source2p)*fact2; \
	destp++; source1p++; source2p++; \
	*destp = (*source1p)*fact1 + (*source2p)*fact2; \
	destp++; source1p++; source2p++; \
	*destp = (*source1p)*fact1 + (*source2p)*fact2; \
	destp++; source1p++; source2p++; \
} while (0)

#define SCALARPROD1_3_d(destp, source1p, source2p, source3p, fact1, fact2, fact3) \
do { \
	*destp = (*source1p)*fact1 + (*source2p)*fact2 + (*source3p)*fact3; \
	destp++; source1p++; source2p++; source3p++; \
} while (0)
#define SCALARPROD4_3_d(destp, source1p, source2p, source3p, fact1, fact2, fact3) \
do { \
	*destp = (*source1p)*fact1 + (*source2p)*fact2 + (*source3p)*fact3; \
	destp++; source1p++; source2p++; source3p++; \
	*destp = (*source1p)*fact1 + (*source2p)*fact2 + (*source3p)*fact3; \
	destp++; source1p++; source2p++; source3p++; \
	*destp = (*source1p)*fact1 + (*source2p)*fact2 + (*source3p)*fact3; \
	destp++; source1p++; source2p++; source3p++; \
	*destp = (*source1p)*fact1 + (*source2p)*fact2 + (*source3p)*fact3; \
	destp++; source1p++; source2p++; source3p++; \
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
