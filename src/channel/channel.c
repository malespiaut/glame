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
#include "util.h"
#include "channel.h"


/* the channel group */
struct cg_s {
	struct hash_head hash;
	struct list_head ch_list;

	char *cg_name;
	void *namespace;

	int nr_channels;
};

/* channel-group hash */
#define hash_find_cg(group) __hash_entry(_hash_find((group), CG_NAMESPACE, _hash((group), CG_NAMESPACE), __hash_pos(cg_t, hash, cg_name, namespace)), cg_t, hash)
#define hash_add_cg(cg) _hash_add(&(cg)->hash, _hash((cg)->cg_name, CG_NAMESPACE))
#define hash_remove_cg(cg) _hash_remove(&(cg)->hash)
#define hash_init_cg(cg) do { cg->namespace = CG_NAMESPACE; _hash_init(&(cg)->hash); } while (0)
#define is_hashed_cg(cg) _is_hashed(&(cg)->hash)

/* channel hash */
#define hash_find_channel(name, group) __hash_entry(_hash_find((name), (group), _hash((name), (group)), __hash_pos(channel_t, hash, ch_name, cg)), channel_t, hash)
#define hash_add_channel(c) _hash_add(&(c)->hash, _hash((c)->ch_name, (c)->cg))
#define hash_remove_channel(c) _hash_remove(&(c)->hash)
#define hash_init_channel(c) _hash_init(&(c)->hash)
#define is_hashed_channel(c) _is_hashed(&(c)->hash)




static cg_t *cg_create(const char *name)
{
  cg_t *cg;

  if (hash_find_cg(name)) /* name should be unique ? */
    return NULL;

  if (!(cg = (cg_t *)malloc(sizeof(cg_t))))
    return NULL;
  memset(cg, 0, sizeof(cg_t));
  hash_init_cg(cg);
  cg->nr_channels = 0;

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
  if (cg->nr_channels || !list_empty(&cg->ch_list))
    return;

  hash_remove_cg(cg);

  free(cg->cg_name);
  free(cg);
}



/* public API */

int init_channel()
{
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

  if (!(c = ALLOC(channel_t)))
    return -1;

  if (!(c->ch_name = strdup(chan)))
    goto _notrackname;
  hash_init_channel(c);

  c->fid=fid;
  c->type=type;
  c->freq=freq;
  c->cg = cg;
  
  list_add(&c->ch_list, &cg->ch_list);
  cg->nr_channels++;
  hash_add_channel(c);

  return 0;

_notrackname:
  free(c);
  return -1;
}

channel_t *get_channel(const char *group, const char *chan)
{
  cg_t *cg;

  if (!group || !chan)
    return NULL;

  if (!(cg = hash_find_cg(group)))
    return NULL;

  return hash_find_channel(chan, cg);
}

int remove_channel(channel_t *chan)
{
  if (!chan)
    return -1;

  chan->cg->nr_channels--;
  list_del(&chan->ch_list);
  hash_remove_channel(chan);

  file_unref(chan->fid);

  /* try to kill the channel group */
  cg_free(chan->cg);

  free(chan);

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
  if (!chan)
    return NULL;

  if (chan->ch_list.next == &chan->cg->ch_list)
    return NULL;

  return list_entry(chan->ch_list.next, channel_t, ch_list);
}

int num_channel(const char *group)
{
  cg_t *cg;
  
  if (!group)
    return 0;

  if (!(cg = hash_find_cg(group)))
    return 0;

  return cg->nr_channels;
}
