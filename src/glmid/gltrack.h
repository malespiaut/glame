#ifndef _GLTRACK_H
#define _GLTRACK_H

/*
 * gltrack.h
 * $Id: gltrack.h,v 1.3 2000/03/25 15:03:56 richi Exp $
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

#include <list.h>
#include <glame_hash.h>
#include <glame_types.h>
#include <swapfile.h>


struct tg_s;
typedef struct tg_s tg_t;

struct track_s;
typedef struct track_s track_t;

struct track_s {
	struct list_head ch_list;
	struct hash_head hash;

	char *ch_name; /* track name */
	tg_t *cg;      /* track group */

	int rate;      /* SAMPLEs per second */ 
	float hangle;  /* position, [-pi, pi] */

	fileid_t fid;  /* swapfile->file holding rawdata */
	float offset;  /* track offset on timeline (ms) */
};
#define track_size(chan) (file_size((chan)->fid))
#define track_rate(chan) ((chan)->rate)
#define track_hangle(chan) ((chan)->hangle)
#define track_name(chan) ((const char *)((chan)->name))
#define track_fid(chan) ((chan)->fid)
#define track_offset(chan) ((chan)->offset)



/* adds track to track group.
 * returns -1 on error
 */
int track_add(const char *group, const char *track,
	      int fid, int rate, float hangle, float offset);

/* get track by track group name & track name, if name == NULL
 * the first track of the group is returned */
track_t *track_get(const char *group, const char *track);

/* get (traverse) tracks by traversing the track group */
track_t *track_next(track_t *track);

/* number of tracks in track group */
int track_cnt(const char *group);

/* delete track from track group */
int track_delete(track_t *track);


int track_set_hangle(track_t *track, float hangle);
int track_set_offset(track_t *track, float offset);


#endif
