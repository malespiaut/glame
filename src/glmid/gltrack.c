/*
 * gltrack.c
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
#include "gltrack.h"


/* the track group */
struct tg_s {
	struct hash_head hash;
	struct list_head ch_list;

	char *tg_name;
	void *namespace;

	int nr_tracks;
};

/* track-group hash */
#define hash_find_cg(group) __hash_entry(_hash_find((group), TG_NAMESPACE, _hash((group), TG_NAMESPACE), __hash_pos(tg_t, hash, tg_name, namespace)), tg_t, hash)
#define hash_add_cg(cg) _hash_add(&(cg)->hash, _hash((cg)->tg_name, TG_NAMESPACE))
#define hash_remove_cg(cg) _hash_remove(&(cg)->hash)
#define hash_init_cg(cg) do { cg->namespace = TG_NAMESPACE; _hash_init(&(cg)->hash); } while (0)
#define is_hashed_cg(cg) _is_hashed(&(cg)->hash)

/* track hash */
#define hash_find_track(name, group) __hash_entry(_hash_find((name), (group), _hash((name), (group)), __hash_pos(track_t, hash, ch_name, cg)), track_t, hash)
#define hash_add_track(c) _hash_add(&(c)->hash, _hash((c)->ch_name, (c)->cg))
#define hash_remove_track(c) _hash_remove(&(c)->hash)
#define hash_init_track(c) _hash_init(&(c)->hash)
#define is_hashed_track(c) _is_hashed(&(c)->hash)




static tg_t *tg_create(const char *name)
{
	tg_t *cg;

	if (hash_find_cg(name)) /* name should be unique ? */
		return NULL;

	if (!(cg = (tg_t *)malloc(sizeof(tg_t))))
		return NULL;
	memset(cg, 0, sizeof(tg_t));
	hash_init_cg(cg);
	cg->nr_tracks = 0;

	if (!(cg->tg_name = strdup(name)))
		goto _nostring;

	INIT_LIST_HEAD(&cg->ch_list);
	hash_add_cg(cg);

	return cg;

_nostring:
	free(cg);
	return NULL;
}

static void tg_free(tg_t *cg)
{
	if (cg->nr_tracks || !list_empty(&cg->ch_list))
		return;

	hash_remove_cg(cg);

	free(cg->tg_name);
	free(cg);
}



/* public API */

int init_track()
{
	return 0;
}

int add_track(const char *group, const char *chan,
	      int fid, int type, int freq)
{
	track_t *c;
	tg_t *cg;

	if (!group || !chan)
		return -1;

	if (!(cg = hash_find_cg(group))
	    && !(cg = tg_create(group)))
		return -1;

	if (!(c = ALLOC(track_t)))
		return -1;

	if (!(c->ch_name = strdup(chan)))
		goto _notrackname;
	hash_init_track(c);

	c->fid = fid;
	c->type = type;
	c->freq = freq;
	c->cg = cg;
  
	list_add(&c->ch_list, &cg->ch_list);
	cg->nr_tracks++;
	hash_add_track(c);

	return 0;

_notrackname:
	free(c);
	return -1;
}

track_t *get_track(const char *group, const char *chan)
{
	tg_t *cg;

	if (!group || !chan)
		return NULL;

	if (!(cg = hash_find_cg(group)))
		return NULL;

	return hash_find_track(chan, cg);
}

int remove_track(track_t *chan)
{
	if (!chan)
		return -1;

	chan->cg->nr_tracks--;
	list_del(&chan->ch_list);
	hash_remove_track(chan);

	file_unref(chan->fid);

	/* try to kill the track group */
	tg_free(chan->cg);

	free(chan);

	return 0;
}

track_t *get_first_track(const char *group)
{
	tg_t *cg;

	if (!group)
		return NULL;

	if (!(cg = hash_find_cg(group)))
		return NULL;

	return list_gethead(&cg->ch_list, track_t, ch_list);
}

track_t *get_next_track(track_t *chan)
{
	if (!chan)
		return NULL;

	if (chan->ch_list.next == &chan->cg->ch_list)
		return NULL;

	return list_entry(chan->ch_list.next, track_t, ch_list);
}

int num_track(const char *group)
{
	tg_t *cg;

	if (!group)
		return 0;

	if (!(cg = hash_find_cg(group)))
		return 0;

	return cg->nr_tracks;
}
