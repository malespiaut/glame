/*
 * rw.c
 * $Id: rw.c,v 1.2 2000/01/24 10:22:52 richi Exp $
 *
 * Copyright (C) 1999 Richard Guenther
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

#include <sys/types.h>
#include <stdlib.h>
#include <string.h>
#include <regex.h>
#include "rw.h"

static struct list_head rw_list;


static rw_t *rw_creat(const char *name, const char *regexp,
		      int (*read)(const char *, const char *),
		      int (*write)(const char *, const char *))
{
  rw_t *rw;

  if (!name)
    return NULL;

  if (!(rw = (rw_t *)malloc(sizeof(rw_t))))
    return NULL;
  memset(rw, 0, sizeof(rw_t));

  strncpy(rw->name, name, 31);

  /* default regexp */
  if (!regexp)
    strncpy(rw->regexp, ".*", 31);
  else
    strncpy(rw->regexp, regexp, 127);

  rw->read = read;
  rw->write = write;

  return rw;
}

static int rw_add(rw_t *rw)
{
  if (!rw)
    return -1;

  list_add(&rw->rw_list, &rw_list);

  return 0;
}

static rw_t *rw_find_next(const char *filename, rw_t *rw)
{
  regex_t rex;
  int res = -1;

  do {
    if (rw)
      rw = list_entry(rw->rw_list.next, rw_t, rw_list);
    else
      rw = list_entry(rw_list.next, rw_t, rw_list);
    if (&rw->rw_list == &rw_list)
      return NULL;

    if (regcomp(&rex, rw->regexp, REG_EXTENDED|REG_ICASE|REG_NOSUB) == -1)
      continue;

    res = regexec(&rex, filename, 0, NULL, 0);

    regfree(&rex);
  } while (res != 0);

  return res == 0 ? rw : NULL;
}


/* internal API */

int rw_get_opts(const char *group, rw_opt_t *opt)
{
  /* FIXME: this needs some thought.
   * think of a group with different channels - how do we fix this?
   * and think of more useful opt entries.
   * finally somebody has to implement it :)
   */
  return -1;
}


/* public API */

int rw_init()
{
  rw_t *rw;

  INIT_LIST_HEAD(&rw_list);

  /* FIXME: add all known rw's here */
  rw = rw_creat("wav", ".*\\.wav", wav_read, wav_write);
  rw_add(rw);

  return 0;
}

int rw_read(const char *filename, const char *chanfilename)
{
  rw_t *rw = NULL;

  while ((rw = rw_find_next(filename, rw))) {
    if (!rw->read)
      continue;
    if (rw->read(filename, chanfilename) == 0)
      return 0;
  }

  return -1;
}

int rw_write(const char *filename, const char *chanfilename)
{
  rw_t *rw = NULL;

  while ((rw = rw_find_next(filename, rw))) {
    if (!rw->write)
      continue;
    if (rw->write(filename, chanfilename) == 0)
      return 0;
  }

  return -1;
}
