/*
 * importexport.c
 * $Id: importexport.c,v 1.4 2001/07/16 09:51:19 richi Exp $
 *
 * Copyright (C) 2001 Alexander Ehlert
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
#include <gtk/gtk.h>
#include <gnome.h>
#include "filter.h"
#include "util.h"
#include "glplugin.h"
#include "gpsm.h"
#include "swapfile.h"
#include "util/glame_gui_utils.h"

#ifdef HAVE_AUDIOFILE
#include "audiofile.h"

PLUGIN_SET(importexport, "import export")

extern GtkWidget *glame_appbar;

typedef struct {
	long	filename;
	swfd_t	fd;
	struct sw_stat stat;
	SAMPLE *os, *s;
	int	size, done;
} swfchan_t;

static int export_gpsm(gpsm_item_t *item, long start, long length) 
{
	return 0;
}

static int import_gpsm(gpsm_item_t *item, long start, long length) 
{
	GtkWidget	*dialog;
	char		*filenamebuffer;
	gpsm_grp_t	*group;
	gpsm_item_t	*it;
	int 		i, j, err = -1, todo;
	swfchan_t	*chans;

	/* Audiofile stuff */

	AFfilehandle	afile;
	AFframecount	aframes, pframes, framecnt;
	AFfilesetup	fsetup;
	int		channels;
	int		rate, cnt, framesize, buffersize;
	float		hangle, dh;
	SAMPLE		*buffer;
	SAMPLE		**p;

	GtkProgress	*procbar;
	
	if (!(GPSM_ITEM_IS_GRP(item)))
		return -1;
	
	filenamebuffer = alloca(256);

	/* Query the file name. */
	dialog = glame_dialog_file_request("Import audio file", 
			"swapfilegui:import", "Filename", NULL, filenamebuffer); 
	
	if(!gnome_dialog_run_and_close(GNOME_DIALOG(dialog))) 
		return -1;

	afile = afOpenFile(filenamebuffer,"r", NULL);
	if (afile == NULL)
		return -1;

	/* Let's just assume that float has 32 bits */
	afSetVirtualSampleFormat(afile, AF_DEFAULT_TRACK, AF_SAMPFMT_FLOAT, 32);
	/* This has to be called after setting the virtual sample format */
	afSetVirtualPCMMapping(afile, AF_DEFAULT_TRACK, 1.0, 0.0, -1.0, 1.0);

	pframes = aframes = afGetFrameCount(afile, AF_DEFAULT_TRACK);
	channels = afGetChannels(afile, AF_DEFAULT_TRACK);
	DPRINTF("channels = %d\n", channels);
	DPRINTF("frames   = %ld\n", aframes);
	
	rate = (int)afGetRate(afile, AF_DEFAULT_TRACK);
	DPRINTF("sample rate = %d\n", rate);
	
	framesize = afGetVirtualFrameSize(afile, AF_DEFAULT_TRACK, 0);
	DPRINTF("frame size = %d bytes\n", framesize);

	group = gpsm_newgrp(g_basename(filenamebuffer));
	
	if (channels == 1)
	{
		/* center/mono position */
		hangle = FILTER_PIPEPOS_CENTRE;
		dh = 0.0;
	}
	else
	{
		/* just distribute n channels horizontally from -pi/2 to pi/2 */
		hangle = FILTER_PIPEPOS_LEFT;
		dh = M_PI/(float)(channels-1);
	}
	
	buffersize = (32768/framesize)*framesize;
	cnt 	 = buffersize / framesize;
	DPRINTF("Allocating deinterleave buffer of size %d\n", buffersize);
	buffer	 = ALLOCN(buffersize, SAMPLE);
	chans	 = ALLOCN(channels, swfchan_t);
	/*
	p	 = ALLOCN(SAMPLE *, channels);
	*/

	for (i=0; i<channels; i++) {
		char swfilename[256]; 
		snprintf(swfilename, 255, "track-%i", i); 
		if (!(it = (gpsm_item_t *)gpsm_newswfile(swfilename))) 
			goto fail_cleanup; 
		
		chans[i].filename = gpsm_swfile_filename(it);
		chans[i].fd = sw_open(chans[i].filename, O_RDWR);
		sw_ftruncate(chans[i].fd, aframes * SAMPLE_SIZE);
		sw_fstat(chans[i].fd, &(chans[i].stat));
		chans[i].done = 0;
		chans[i].size = chans[i].stat.cluster_size/SAMPLE_SIZE;
		chans[i].os = chans[i].s = (SAMPLE *) sw_mmap(0, PROT_READ | PROT_WRITE, 
								MAP_SHARED, chans[i].fd);

		gpsm_item_place(group, it, 0, i);
		gpsm_swfile_set((gpsm_swfile_t *)it, rate, hangle); 
		hangle += dh;
	}

	procbar = gnome_appbar_get_progress(GNOME_APPBAR(glame_appbar));
	gnome_appbar_set_status(GNOME_APPBAR(glame_appbar), "Loading file...");
	gtk_progress_set_show_text(GTK_PROGRESS (procbar), TRUE);

	while(aframes > 0) {
		SAMPLE *s;
		framecnt = afReadFrames(afile, AF_DEFAULT_TRACK, buffer, MIN(cnt, aframes));
		aframes -= framecnt;
		s = buffer;

		gtk_progress_bar_update(GTK_PROGRESS_BAR(procbar), 
					1.0-(double)aframes/(double)pframes);

		while (gtk_events_pending())
			gtk_main_iteration();
		
		while (framecnt>0) {
			
			todo = framecnt;
			for (i=0; i < channels; i++)
				todo = MIN(todo, chans[i].size-chans[i].done);
			
			if (todo == 0) {
				DPRINTF("Guru Meditation: Something wonderful has happened\n");
				break;
			}
			/* deinterleave channels */
			for (i=0; i<todo; i++)
				for(j=0; j<channels; j++)
					*chans[j].s++ = *s++;
			
			/* adjust swapfiles */
			for (i=0;i<channels;i++) {
				swfchan_t *c = &chans[i];
				c->done += todo;
				if (c->done == c->size) {
					sw_munmap(c->os);
					sw_lseek(c->fd, c->stat.cluster_size, SEEK_CUR);
					sw_fstat(c->fd, &(c->stat));
					c->done = c->size = 0;
					
					if (aframes == 0)
						continue;
					
					if (c->stat.cluster_size == 0) {
						DPRINTF("FATAL: Ran into empty cluster\n");
						continue;
					}
						
					c->size = c->stat.cluster_size/SAMPLE_SIZE;
					c->os = c->s = (SAMPLE *) sw_mmap(0, 
							PROT_READ | PROT_WRITE, 
							MAP_SHARED, c->fd);
				}
			}
			framecnt -= todo;
		}
		
	}
	/* set main window back to normal */
	gnome_appbar_pop(GNOME_APPBAR(glame_appbar));
	gtk_progress_bar_update(GTK_PROGRESS_BAR(procbar), 0.0);

	/* Close all files */
	for (i=0;i<channels;i++)
		sw_close(chans[i].fd);
	free(chans);

	/* Notify gpsm of the change. */
	gpsm_grp_foreach_item(group, it) 
		gpsm_invalidate_swapfile(gpsm_swfile_filename(it));

	/* Insert the group into the gpsm tree. */ 
	gpsm_item_place((gpsm_grp_t *)item, (gpsm_item_t *)group,
			0, gpsm_item_vsize(item));
	
	gpsm_sync();

	err = 0;
fail_cleanup:
	free(buffer);
	afCloseFile(afile);
	return err;
}

int export_register(plugin_t *p)
{
	plugin_set(p, PLUGIN_CATEGORY, "ImEx");
  	plugin_set(p, PLUGIN_GPSMOP, export_gpsm);
	plugin_set(p, PLUGIN_LABEL, "Export");
	return 0;
}

int import_register(plugin_t *p)
{
	plugin_set(p, PLUGIN_CATEGORY, "ImEx");
  	plugin_set(p, PLUGIN_GPSMOP, import_gpsm);
	plugin_set(p, PLUGIN_LABEL, "Import");
	return 0;
}
#endif
