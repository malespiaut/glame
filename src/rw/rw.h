#ifndef _RW_H
#define _RW_H

/*
 * rw.h
 * $Id: rw.h,v 1.2 2000/01/24 10:22:52 richi Exp $
 *
 * Copyright (C) 1999, 2000 Richard Guenther
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
#include <sys/types.h>

struct rw_s;
typedef struct rw_s rw_t;

struct rw_opt_s;
typedef struct rw_opt_s rw_opt_t;

struct rw_s {
  struct list_head rw_list;

  /* name of the reader/writer ("wav", "au", etc.) */
  char name[32];

  /* filename regular expression that matches filenames
   * which are likely to be handled by this readerwriter.
   * used with regcomp/regexec & friends, so use their
   * subset of regular expression syntax! (like '.*\.wav')
   */
  char regexp[128];

  int (*read)(const char *filename, const char *group);
  int (*write)(const char *filename, const char *group);
};


/* These are very common options for lots of fileformats. 
 * They can get the information using rw_get_opts() on
 * the channel group.
 */
struct rw_opt_s {
	ushort mode;		/* 1 = Mono, 2 = Stereo */
	ulong  freq;		/* Samples/Second       */
	ushort byte_p_smp;	/* Bytes/Sample		*/
	ushort bit_p_smp;	/* Bits/Sample		*/
};

#include "wav_file.h"

/* internal helpers to all rw implementers */
int rw_get_opts(const char *group, rw_opt_t *opt);




/* init rw-list, inserts all known handlers */
int rw_init();


/* load a file into a channel-file */
int rw_read(const char *filename, const char *group);

/* write a file into a channel-file */
int rw_write(const char *filename, const char *group);



#endif
