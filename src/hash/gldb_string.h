#ifndef _GLDB_STRING_H
#define _GLDB_STRING_H

#include "gldb.h"

typedef struct {
	gldb_item_t entry;
	char *str;
} sitem_t;

#define sitem_label(si) ((si)->entry.label)
#define sitem_str(si) ((si)->str)

void glsdb_init(gldb_t *db);

#define glsdb_delete(db) gldb_delete(db)
#define glsdb_copy(d, s) gldb_copy((d), (s))

#define glsdb_foreach_item(pdb, i) list_foreach(&(pdb)->item, sitem_t, entry.list, i)

void glsdb_set(gldb_t *db, const char *str, const char *label);
void glsdb_remove(gldb_t *db, const char *label);
char *glsdb_query(gldb_t *db, const char *label);


#endif
