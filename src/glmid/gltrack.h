#ifndef _GLTRACK_H
#define _GLTRACK_H

/*
 * gltrack.h
 * $Id: gltrack.h,v 1.1 2000/03/15 13:06:16 richi Exp $
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


#define TRACK_NUM_LEFT      0
#define TRACK_NUM_RIGHT     1
#define TRACK_NUM_CENTER    2
#define TRACK_NUM_MISC     98
#define TRACK_NUM_FLOATING 99


struct tg_s;
typedef struct tg_s tg_t;

struct track_s;
typedef struct track_s track_t;

struct track_s {
	struct list_head ch_list;
	struct hash_head hash;

	char *ch_name; /* track name */
	tg_t *cg;      /* track group */

	int type;      /* left,right,center,floating, etc. */
	int freq;      /* SAMPLEs per second         */ 

	fileid_t fid;  /* swapfile->file holding rawdata */
};
#define track_size(chan) (file_size((chan)->fid))
#define track_type(chan) ((chan)->type)
#define track_freq(chan) ((chan)->freq)
#define track_name(chan) ((const char *)((chan)->name))
#define track_fid(chan) ((chan)->fid)



/* init the track subsystem */
int init_track();


/* adds track to track group.
 * returns -1 on error
 */
int add_track(const char *group, const char *chan,
	      int fid, int type, int freq);

/* remove track from track group */
int remove_track(track_t *chan);


/* get (traverse) tracks by traversing the track group */
track_t *get_first_track(const char *group);
track_t *get_next_track(track_t *chan);

/* get track by track group name & track name */
track_t *get_track(const char *group, const char *chan);

/* number of tracks in track group */
int num_track(const char *group);

#endif
