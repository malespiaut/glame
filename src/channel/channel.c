/*
 * channel.c
 *
 * Copyright (C) 1999, 2000 Alexander Ehlert, Richard Guenther
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
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "channel.h"


struct cg_s;
typedef struct cg_s cg_t;

struct cg_s {
  cg_t *next_hash;
  cg_t **pprev_hash;
  struct list_head ch_list;
  char *cg_name;
};

cg_t **cg_hash_table = NULL;
#include "channel_cghash.h"



static cg_t *cg_create(const char *name)
{
  cg_t *cg;
  
  if (hash_find_cg(name)) /* name should be unique ? */
    return NULL;

  if (!(cg = (cg_t *)malloc(sizeof(cg_t))))
    return NULL;

  memset(cg, 0, sizeof(cg_t));

  if (!(cg->cg_name = strdup(name)))
    goto _nostring;

  INIT_LIST_HEAD(&cg->ch_list);
  hash_add_cg(cg);

  return cg;

_nostring:
  free(cg);
  return NULL;
}

static void cg_free(cg_t *cg)
{
  if (!list_empty(&cg->ch_list))
    return;

  hash_remove_cg(cg);

  free(cg->cg_name);
  free(cg);
}



/* public API */

int init_channel()
{
  hash_alloc_cg();

  return 0;
}

int add_channel(const char *group, const char *chan,
		int fid, int type, int freq)
{
  channel_t *c;
  cg_t *cg;

  if (!group || !chan)
    return -1;

  if (!(cg = hash_find_cg(group))
      && !(cg = cg_create(group)))
    return -1;

  if (!(c=(channel_t *)malloc(sizeof(channel_t))))
    return -1;
  
  if (!(c->ch_name = strdup(chan)))
    goto _notrackname;

  c->fid=fid;
  c->type=type;
  c->freq=freq;
  c->cg_name = cg->cg_name;
  
  list_add(&c->ch_list, &cg->ch_list);
  return 0;

_notrackname:
  free(c);
  return -1;
}

channel_t *get_channel(const char *group, const char *chan)
{
  cg_t *cg;
  channel_t *c;

  if (!group || !chan)
    return NULL;

  if (!(cg = hash_find_cg(group)))
    return NULL;

  list_foreach(&cg->ch_list, channel_t, ch_list, c,
	       if (strcmp(c->ch_name, chan)!=0) return c);

  return NULL;
}

int remove_channel(channel_t *chan)
{
  cg_t *cg;

  if (!chan)
    return -1;

  if (!(cg = hash_find_cg(chan->cg_name)))
    return -1;

  list_del(&chan->ch_list);

  file_unref(chan->fid);
  free(chan);

  /* try to kill the channel group */
  cg_free(cg);

  return 0;
}

channel_t *get_first_channel(const char *group)
{
  cg_t *cg;

  if (!group)
    return NULL;

  if (!(cg = hash_find_cg(group)))
    return NULL;

  return list_gethead(&cg->ch_list, channel_t, ch_list);
}

channel_t *get_next_channel(channel_t *chan)
{
  cg_t *cg;

  if (!chan)
    return NULL;

  if (!(cg = hash_find_cg(chan->cg_name)))
    return NULL;

  if (chan->ch_list.next == &cg->ch_list)
    return NULL;

  return list_entry(chan->ch_list.next, channel_t, ch_list);
}

int num_channel(const char *group)
{
  cg_t *cg;
  channel_t *chan;
  int num=0;
  
  if (!group)
    return 0;

  if (!(cg = hash_find_cg(group)))
    return 0;
	    
  list_foreach(&cg->ch_list, channel_t, ch_list, chan, num++);

  return num;
}
