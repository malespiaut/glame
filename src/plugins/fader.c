/*
 * fade.c
 * $Id: fader.c,v 1.4 2004/10/23 13:09:29 richi Exp $
 *
 * Copyright (C) 2001, 2002, 2003, 2004 Alexander Ehlert
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
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "filter.h"
#include "util.h"
#include "glplugin.h"
#include "gpsm.h"
#include "swapfile.h"

PLUGIN_SET(fader, "fadein fadeout")

static int fade(gpsm_item_t *item, long start, long length, int in)
{
	gpsm_grp_t	*grp;
	gpsm_item_t	*file;
	
	long		pre_off;
	float		gain, dg, sgain;

	dg 	= 1.0 / (float)length;
	sgain 	= 0.0;

	/* fade out or fade in is the question */

	if (in == 0) {
		sgain = 1.0;
		dg = -dg;
	}

	DPRINTF("now fade from %ld to %ld\n", start, start+length);
	
	gpsm_op_prepare(item);
	
	grp=gpsm_collect_swfiles(item);
	if (grp == NULL)
		return -1;

	gpsm_grp_foreach_item(grp, file) {
		swfd_t fd;
		struct sw_stat stat;
		SAMPLE *s, *os;
		long max, done, fstart, fend, pos;
		size_t cnt;
		int offset;
		
		fstart = start - gpsm_item_hposition(file);
		fend = fstart + length;
		if (fend < 0 || fstart >= gpsm_item_hsize(file))
			continue;
		
		/* how many samples have been faded already */
		
		if (fstart < 0) {
			pre_off = -fstart;
			fstart = 0;
		}
		else
			pre_off = 0;
		
		
		gain = sgain + (float)pre_off * dg;
		fd   = sw_open(gpsm_swfile_filename(file), O_RDWR);
		done = 0;
		pos  = fstart*SAMPLE_SIZE;
		max  = MIN(fend, gpsm_item_hsize(file)) - fstart;
		
		while (done < max) {
			sw_lseek(fd, pos, SEEK_SET);
			sw_fstat(fd, &stat);
		
			offset = pos - stat.cluster_start;
			
			os = s = (SAMPLE *) sw_mmap(0, PROT_READ | PROT_WRITE, 
					       MAP_SHARED, fd);
			s += offset/SAMPLE_SIZE;
			cnt = stat.cluster_size - offset;
			pos += cnt;

			cnt /= SAMPLE_SIZE;
			done += cnt;
			if (done > max)
				cnt -= done - max;
			
			while (cnt--) {
				*s++ *= gain;
				gain += dg;
			}
			sw_munmap(os);
		}
		sw_close(fd);
		gpsm_notify_swapfile_change(gpsm_swfile_filename(file), 
					    fstart, done);
	}
	return 0;
}

static int fadein_gpsm(gpsm_item_t *item, long start, long length) 
{ 
	return fade(item, start, length, 1);
}

static int fadeout_gpsm(gpsm_item_t *item, long start, long length) 
{
	return fade(item, start, length, 0);
}

int fadein_register(plugin_t *p)
{
	plugin_set(p, PLUGIN_CATEGORY, "Volume");
	plugin_set(p, PLUGIN_DESCRIPTION, "do a linear volume" 
			" envelope for selection");
  	plugin_set(p, PLUGIN_GPSMOP, fadein_gpsm);
	plugin_set(p, PLUGIN_LABEL, "Fade In");
	return 0;
}

int fadeout_register(plugin_t *p)
{
	plugin_set(p, PLUGIN_CATEGORY, "Volume");
	plugin_set(p, PLUGIN_DESCRIPTION, "do a linear volume" 
			" envelope for selection");
  	plugin_set(p, PLUGIN_GPSMOP, fadeout_gpsm);
	plugin_set(p, PLUGIN_LABEL, "Fade Out");
	return 0;
}
