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


/* Here follows a set of fast computing macros for standard operations.
 * To be implemented using ISSE/3DNOW stuff if available.
 */

#define SCALARPROD1_1_CLAMP(destsourcep, fact) \
do { \
	*destsourcep = (*destsourcep)*fact; \
        if (*destsourcep<-1.0) *destsourcep = -1.0; \
        else if (*destsourcep>1.0) *destsourcep = 1.0; \
        destsourcep++; \
} while (0)

#define SCALARPROD4_1_CLAMP(destsourcep, fact) \
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


#endif
