#ifndef _GLDB_WORM_H
#define _GLDB_WORM_H

#include "gldb.h"

/* Simple general purpose write once, read many database
 * for arbitrary (void * style) data.
 * Write once, read many means you may add an item and
 * after that never modify or remove it. This way shared
 * values between multiple databases works.
 */

typedef struct {
	gldb_item_t item;

	union {
		void *ptr;
		char *str;
		int i;
		float f;
	} u;
} glworm_t;


void glwdb_init(gldb_t *db);

glworm_t *glworm_alloc();

int glwdb_add_item(gldb_t *db, glworm_t *item, const char *label);
const glworm_t *glwdb_query_item(gldb_t *db, const char *label);

#define glwdb_foreach_item(db, i) list_foreach(&(db)->items, glworm_t, item.list, i)


#endif
