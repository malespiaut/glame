#ifndef _CHANNEL_H
#define _CHANNEL_H

/*
 * channel.h
 * $Id: channel.h,v 1.1 2000/01/20 14:54:19 richi Exp $
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

#include "list.h"
#include "glame_types.h"
#include "swapfile.h"


#define CHANNEL_NUM_LEFT      0
#define CHANNEL_NUM_RIGHT     1
#define CHANNEL_NUM_CENTER    2
#define CHANNEL_NUM_MISC     98
#define CHANNEL_NUM_FLOATING 99


struct channel_s;
typedef struct channel_s channel_t;

struct channel_s {
  struct list_head ch_list;

  char *ch_name; /* channel name */
  char *cg_name; /* channel group name */

  int type;      /* left,right,center,floating, etc. */
  int freq;      /* SAMPLEs per second         */ 

  fileid_t fid;  /* swapfile->file holding rawdata */
};
#define channel_size(chan) (file_size((chan)->fid))
#define channel_type(chan) ((chan)->type)
#define channel_freq(chan) ((chan)->freq)
#define channel_name(chan) ((const char *)((chan)->name))
#define channel_fid(chan) ((chan)->fid)



/* init the channel subsystem */
int init_channel();


/* adds channel to channel group.
 * returns -1 on error
 */
int add_channel(const char *group, const char *chan,
		int fid, int type, int freq);

/* remove channel from channel group */
int remove_channel(channel_t *chan);


/* get (traverse) channels by traversing the channel group */
channel_t *get_first_channel(const char *group);
channel_t *get_next_channel(channel_t *chan);

/* get channel by channel group name & channel name */
channel_t *get_channel(const char *group, const char *chan);

/* number of channels in channel group */
int num_channel(const char *group);

#endif


