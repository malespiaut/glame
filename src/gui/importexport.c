/*
 * importexport.c
 * $Id: importexport.c,v 1.27 2003/07/03 20:17:52 nold Exp $
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
#include <libgnomevfs/gnome-vfs-mime-utils.h>
#include <gnome.h>
#include "filter.h"
#include "util.h"
#include "glplugin.h"
#include "gpsm.h"
#include "swapfile.h"
#include "util/glame_gui_utils.h"
#include "network_utils.h"
#include "waveeditgui.h"
#include "glame_audiofile.h"
#ifdef HAVE_LIBMAD
#include <mad.h>
#endif
#ifdef HAVE_LIBVORBISFILE
#include <vorbis/codec.h>
#include <vorbis/vorbisfile.h>
#endif


     
static char *ftlabel[] = { "raw", "aiffc", "aiff", "nextsnd", "wav", "sf", "ogg", "mp3" };
static char *sflabel[] = { "16 bit signed", "24 bit signed", "32 bit signed", 
			   "8 bit unsigned", "32 bit float", "64 bit float" };
static long sf_format[] = { AF_SAMPFMT_TWOSCOMP, AF_SAMPFMT_TWOSCOMP, AF_SAMPFMT_TWOSCOMP,
			   AF_SAMPFMT_UNSIGNED, AF_SAMPFMT_FLOAT, AF_SAMPFMT_DOUBLE };
static long sf_width[] = { 16, 24, 32, 8, 32, 64 };

static char *rlabel[] = { "original", "mono", "stereo" };

#define MAX_RLABEL (sizeof(rlabel)/sizeof(char*))
#define MAX_SFLABEL (sizeof(sflabel)/sizeof(char*))


static char *fproplabel[] = { "Format", "Samplerate", "Quality", "Channels", 
			      "Duration", "Compression", "Max RMS", "DC Offset"
};

#define MAX_PROPS (sizeof(fproplabel)/sizeof(char*))

#define IMPORT  0
#define OK      0
/*#define PREVIEW 1*/
#define CANCEL  1
#define HELP    2


/*
 * Import.
 */

struct imp_s {
	filter_t *net;
	filter_launchcontext_t *context;
	int cancelled;
	int importing;
	int destroyed;
	gpsm_item_t *item;
	GtkWidget *dialog;
	GtkWidget *edit, *appbar, *checkresample, *rateentry, *getstats;
	filter_t *readfile;
	gchar *filename;
	unsigned int frames;
	long rate;
	int chancnt, gotfile, gotstats;
	float maxrms, dcoffset;
	plugin_t *resample;
	GtkWidget *fi_plabel[MAX_PROPS];
	int reallydone;
#ifdef HAVE_LIBMAD
	int mad_length;
	int mad_pos;
	char *mad_buffer;
#endif
};


static void ie_import_cleanup(struct imp_s *ie) 
{
	if (ie->dialog)
		gnome_dialog_close(GNOME_DIALOG(ie->dialog));

	if (ie->readfile!=NULL)
		filter_delete(ie->readfile);

	ie->reallydone = 1;
	/* free(ie);  -- done in glame_import_dialog() */
}

static void ie_cancel_cb(GtkWidget *bla, struct imp_s *ie)
{
	DPRINTF("cancel pressed\n");
	if (ie->importing==1) {
		filter_terminate(ie->context);
		ie->cancelled = 1;
		DPRINTF("Network was running. Importing terminated.\n");
	}
	else {
		ie_import_cleanup(ie);
		DPRINTF("Nothing running. Just destroyed import dialog.\n");
	}
}

static gint ie_windowkilled(GtkWidget *bla,  GdkEventAny *event, gpointer data)
{
	struct imp_s *ie = (struct imp_s*)data;
	if(ie->importing==1) {
		filter_terminate(ie->context);
	}
	ie->destroyed=1;
	ie->cancelled=1;
	return TRUE;
}

#ifdef HAVE_LIBVORBISFILE
static void ie_import_ogg(struct imp_s *ie)
{
	OggVorbis_File vf;
	vorbis_info *vi;
        FILE *fd;
	gpsm_swfile_t *swfile[2];
	swfd_t swfd[2];
	long ret;
	int i;

	fd = fopen(ie->filename, "r");
	if (fd == NULL)
		return;
	if (ov_open(fd, &vf, NULL, 0) < 0) {
		fclose(fd);
		return;
	}
	vi = ov_info(&vf, -1);
	if (vi->channels > 2) {
		ov_clear(&vf);
		fclose(fd);
		return;
	}
	/* vi->channels, vi->rate */
	/* ov_pcm_total(&vf, -1);  decoded size in samples */

	/* alloc gpsm group, etc. */
	ie->item = (gpsm_item_t *)gpsm_newgrp(g_basename(ie->filename));
	for (i=0; i<vi->channels; ++i) {
		char label[16];
		snprintf(label, 16, "channel-%i", i);
		swfile[i] = gpsm_newswfile(label);
		gpsm_swfile_set(swfile[i], vi->rate,
				vi->channels == 2
				? (i == 0
				   ? FILTER_PIPEPOS_LEFT
				   : FILTER_PIPEPOS_RIGHT)
				: FILTER_PIPEPOS_CENTRE);
		gpsm_item_place((gpsm_grp_t *)ie->item, (gpsm_item_t *)swfile[i], 0, i);
		swfd[i] = sw_open(gpsm_swfile_filename(swfile[i]), O_RDWR);
	}

	/* process file */
	gnome_appbar_set_status(GNOME_APPBAR(ie->appbar), _("Importing..."));
	ie->importing = 1;
	while (1) {
		SAMPLE **ssbuf;
		int current_section;
		/* Vorbis folks, read my lips. Changing calling convention
		 * without changing the function name is calling for trouble.
		 */
		ret = ov_read_float(&vf, &ssbuf, 
#if (GL_OV_READ_FLOAT_ARGS == 4)
		                    GLAME_BULK_BUFSIZE, 
#endif
				    &current_section);
		if (ret <= 0) /* error or EOF */
			break;

		/* append to swapfile */
		for (i=0; i<vi->channels; ++i)
			sw_write(swfd[i], ssbuf[i], SAMPLE_SIZE*ret);

		/* show progress, be friendly to gtk */
		gtk_progress_bar_pulse(gnome_appbar_get_progress(GNOME_APPBAR(ie->appbar)));
		while (gtk_events_pending())
			gtk_main_iteration();
	}

	/* finish gpsm, if no error */
	if (ret == 0) {
		gpsm_item_t *item;
		gpsm_grp_foreach_item(ie->item, item)
			gpsm_invalidate_swapfile(gpsm_swfile_filename(item));
	} else /* if (ret < 0) */ {
		gpsm_item_destroy(ie->item);
		ie->item = NULL;
	}

	/* clean up */
	for (i=0; i<vi->channels; ++i)
		sw_close(swfd[i]);
	ov_clear(&vf);
	fclose(fd);

	gnome_appbar_set_status(GNOME_APPBAR(ie->appbar), _("Done."));
	ie->importing = 0;
	ie_import_cleanup(ie);
}
#endif

#ifdef HAVE_LIBMAD
static enum mad_flow
ie_import_mp3_error(void *data, struct mad_stream *stream,
		    struct mad_frame *frame)
{
	return MAD_FLOW_BREAK;
}
static enum mad_flow
ie_import_mp3_input(void *data, struct mad_stream *stream)
{
	struct imp_s *ie = data;
	if (!ie->mad_length)
		return MAD_FLOW_STOP;
	mad_stream_buffer(stream, ie->mad_buffer, ie->mad_length);
	ie->mad_length = 0;
	return MAD_FLOW_CONTINUE;
}
static enum mad_flow
ie_import_mp3_output(void *data, struct mad_header const *header,
		     struct mad_pcm *pcm)
{
	struct imp_s *ie = data;
	SAMPLE *buf;
	int i;

	if (ie->mad_pos == 0) {
		/* alloc gpsm group, etc. */
		ie->item = (gpsm_item_t *)gpsm_newgrp(g_basename(ie->filename));
		for (i=0; i<pcm->channels; ++i) {
			char label[16];
			gpsm_swfile_t *swfile;
			snprintf(label, 16, "channel-%i", i);
			swfile = gpsm_newswfile(label);
			gpsm_swfile_set(swfile, pcm->samplerate,
					pcm->channels == 2
					? (i == 0
					   ? FILTER_PIPEPOS_LEFT
					   : FILTER_PIPEPOS_RIGHT)
					: FILTER_PIPEPOS_CENTRE);
			gpsm_item_place((gpsm_grp_t *)ie->item, (gpsm_item_t *)swfile, 0, i);
		}
	}

	buf = malloc(SAMPLE_SIZE*pcm->length);
	for (i=0; i<pcm->channels; ++i) {
		unsigned int nsamples = pcm->length;
		mad_fixed_t const *data = pcm->samples[i];
		SAMPLE *b = buf;
		gpsm_swfile_t *file = gpsm_find_swfile_vposition((gpsm_grp_t *)ie->item, NULL, i);
		swfd_t fd;
		fd = sw_open(gpsm_swfile_filename(file), O_RDWR);
		sw_lseek(fd, 0, SEEK_END);

		while (nsamples--)
		    *b++ = mad_f_todouble(*data++);

		sw_write(fd, buf, SAMPLE_SIZE*pcm->length);
		sw_close(fd);
	}
	free(buf);

	ie->mad_pos += pcm->length;

	/* show progress, be friendly to gtk */
	gtk_progress_bar_pulse(gnome_appbar_get_progress(GNOME_APPBAR(ie->appbar)));
	while (gtk_events_pending())
		gtk_main_iteration();

	return MAD_FLOW_CONTINUE;
}
static void ie_import_mp3(struct imp_s *ie)
{
	int fd;
	struct stat s;
	char *buf;
	struct mad_decoder decoder;
	int result;

	fd = open(ie->filename, O_RDONLY);
	if (fd == -1)
		return;
	fstat(fd, &s);
	ie->mad_length = s.st_size;
	ie->mad_buffer = mmap(NULL, s.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
	if (ie->mad_buffer == MAP_FAILED) {
		close(fd);
		return;
	}

	mad_decoder_init(&decoder, ie,
			 ie_import_mp3_input, 0, 0,
			 ie_import_mp3_output,
			 ie_import_mp3_error, 0);
	ie->mad_pos = 0;
	ie->importing = 1;
	gnome_appbar_set_status(GNOME_APPBAR(ie->appbar), _("Importing..."));

	result = mad_decoder_run(&decoder, MAD_DECODER_MODE_SYNC);
	mad_decoder_finish(&decoder);

	munmap(buf, s.st_size);
	close(fd);

	if (result == 0) {
		gpsm_item_t *item;
		gpsm_grp_foreach_item(ie->item, item)
			gpsm_invalidate_swapfile(gpsm_swfile_filename(item));
	} else {
		gnome_dialog_run_and_close(GNOME_DIALOG(
			gnome_error_dialog("Failed importing mp3 file")));
		gpsm_item_destroy(ie->item);
		ie->item = NULL;
	}

	gnome_appbar_set_status(GNOME_APPBAR(ie->appbar), _("Done."));
	ie->importing = 0;
	ie_import_cleanup(ie);
}
#endif

static void ie_import_cb(GtkWidget *bla, struct imp_s *ie)
{
	filter_t *readfile, *swout, *resample;
	filter_port_t *source, *nsource;
	filter_pipe_t *pipe;
	filter_param_t *pos_param;
	float percentage;
	gpsm_grp_t *group = NULL;
	int i;
	long newrate;
	gboolean dorsmpl;
	gpsm_item_t *it;
	GtkWidget *ed;

	if (ie->importing)
		return;

	/* nullify net and set cancel status to zero */
	//ie->net = NULL;
	//ie->importing = 0;

	if(ie->gotfile==0) {
		ed = gnome_error_dialog("Select a file first!");
		gnome_dialog_set_parent(GNOME_DIALOG(ed), GTK_WINDOW(ie->dialog));
		gnome_dialog_run_and_close(GNOME_DIALOG(ed));
		return;
	} else if (ie->gotfile==2) {
#ifdef HAVE_LIBMAD
		ie_import_mp3(ie);
#else
		ed = gnome_error_dialog("Sorry, no mp3 support.\nInstall libmad and rebuild.");
		gnome_dialog_set_parent(GNOME_DIALOG(ed), GTK_WINDOW(ie->dialog));
		gnome_dialog_run_and_close(GNOME_DIALOG(ed));
#endif
		return;
	} else if (ie->gotfile==3) {
#ifdef HAVE_LIBVORBISFILE
		ie_import_ogg(ie);
#else
		ed = gnome_error_dialog("Sorry, no ogg support.\nInstall libvorbisfile and rebuild.");
		gnome_dialog_set_parent(GNOME_DIALOG(ed), GTK_WINDOW(ie->dialog));
		gnome_dialog_run_and_close(GNOME_DIALOG(ed));
#endif
		return;
	}

	DPRINTF("Importing via read-file plugin\n");

	ie->net = filter_creat(NULL);
	ie->importing = 1;
	ie->cancelled = 0;
	if (!(readfile = filter_instantiate(plugin_get("read_file"))))
		return;

	gtk_widget_set_sensitive(bla, FALSE);

	if (filterparam_set(filterparamdb_get_param(filter_paramdb(readfile),
						    "filename"), &(ie->filename))==-1)
		goto ie_fail_cleanup;

	source = filterportdb_get_port(filter_portdb(readfile), PORTNAME_OUT);
	filter_add_node(ie->net, readfile, "readfile");

	dorsmpl = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(ie->checkresample));

	if ((ie->resample) && (dorsmpl)) {
		newrate = gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(ie->rateentry));
		DPRINTF("Resample to %li Hz\n", newrate);
		if(ie->rate==newrate) {
			DPRINTF("new samplerate equals old samplerate\n");
			dorsmpl=FALSE;
		}
	} else
		dorsmpl=FALSE;
		
	/* Setup gpsm group. */
	group = gpsm_newgrp(g_basename(ie->filename));
	
	i = 0;
	do {
		char swfilename[256];
		snprintf(swfilename, 255, "track-%i", i);
		if (!(it = (gpsm_item_t *)gpsm_newswfile(swfilename)))
			goto ie_fail_cleanup;
		gpsm_item_place(group, it, 0, i);
		swout = net_add_gpsm_output(ie->net, (gpsm_swfile_t *)it,
					    0, -1, 3);

		/* Insert resampler here */
		if(dorsmpl) {
			resample = filter_instantiate(ie->resample);
			filter_add_node(ie->net, resample, "resample");
			filterparam_set(filterparamdb_get_param(filter_paramdb(resample),
								"frequency"), &newrate);

			nsource = filterportdb_get_port(filter_portdb(resample),
							PORTNAME_IN);
			if (nsource == NULL)
				DPRINTF("(1)nsource = NULL\n");

			if (!(pipe = filterport_connect(source, nsource))) {
				DPRINTF("Connection to resample failed");
				gpsm_item_destroy(it);
				filter_delete(resample);
				break;
			}
			nsource = filterportdb_get_port(filter_portdb(resample),
							PORTNAME_OUT);
			if(nsource==NULL)
				DPRINTF("(2) nsource = NULL\n");
		} else 
			nsource = source;

			
		if (!(pipe = filterport_connect(
			nsource, filterportdb_get_port(
				filter_portdb(swout), PORTNAME_IN)))) {
			DPRINTF("Connection failed for channel %d\n",i+1);
			gpsm_item_destroy(it);
			filter_delete(swout);
			break;
		}
		gpsm_swfile_set((gpsm_swfile_t *)it,
				filterpipe_sample_rate(pipe),
				filterpipe_sample_hangle(pipe));
		i++;
	} while (i < ie->chancnt);
	
	pos_param = filterparamdb_get_param(filter_paramdb(readfile),
					FILTERPARAM_LABEL_POS);
	
	gnome_appbar_set_status(GNOME_APPBAR(ie->appbar), _("Importing..."));
	

	if (!(ie->context = filter_launch(ie->net, GLAME_BULK_BUFSIZE)) ||
	    (filter_start(ie->context) == -1))
		goto ie_fail_cleanup;

	while(!filter_is_ready(ie->context)) {
		while (gtk_events_pending())
			gtk_main_iteration();
		
		usleep(40000);
		percentage = (float)filterparam_val_long(pos_param)/(float)ie->frames;
		if(percentage>1.0)
			percentage = 1.0;
		gnome_appbar_set_progress_percentage(GNOME_APPBAR(ie->appbar),
					  percentage);
	}
	filter_wait(ie->context);
	filter_delete(ie->net);
	filter_launchcontext_unref(&ie->context);
	ie->net = NULL;
	
	gnome_appbar_set_status(GNOME_APPBAR(ie->appbar), _("Done."));
	gnome_appbar_set_progress_percentage(GNOME_APPBAR(ie->appbar),
				  0.0);

	if (ie->cancelled==0) {
		/* Notify gpsm of the change. */
		gpsm_grp_foreach_item(group, it)
			gpsm_invalidate_swapfile(gpsm_swfile_filename(it));
		
		/* Store the imported gpsm grp for return. */
		ie->item = (gpsm_item_t *)group;
	} else
		gpsm_item_destroy((gpsm_item_t *)group);

	ie_import_cleanup(ie);
	ie->importing = 0;
	return;

ie_fail_cleanup:
	gnome_dialog_run_and_close(GNOME_DIALOG(
		gnome_error_dialog("Failed to create importing network")));
	filter_delete(ie->net);
	ie->net = NULL;
	ie->importing = 0;
	gpsm_item_destroy((gpsm_item_t *)group);
	gtk_widget_set_sensitive(bla, TRUE);	
}

static void ie_preview_cb(GtkWidget *bla, struct imp_s *ie)
{
	DPRINTF("Not implemented\n");
}

static void ie_update_plabels(struct imp_s *ie)
{
	filter_param_t *fparam;
	gchar *property;
	unsigned int frames, minutes, seconds;
	int rate, ftype, version;
	gchar buffer[255];

	DPRINTF("Here\n");
	fparam = filterparamdb_get_param(filter_paramdb(ie->readfile), "filename");
	
	property = filterparam_get_property(fparam, "#format");
	if(property) {
		sscanf(property, "%d %d", &ftype, &version);
		DPRINTF("format property: %s, ftype=%d, version=%d\n", property,ftype,version);
		if ((ftype>=0) && (ftype<8))
			sprintf(buffer, "%s format", ftlabel[ftype]);
		else
			sprintf(buffer, "You shouldn't see this :)");
	} else	
		sprintf(buffer, "nan");

	gtk_label_set_text(GTK_LABEL(ie->fi_plabel[0]), buffer);
	
	property = filterparam_get_property(fparam, "#samplerate");
	gtk_label_set_text(GTK_LABEL(ie->fi_plabel[1]), property);
	if (property) {
		sscanf(property, "%d", &rate);
		ie->rate = rate;
	}
	else
		rate = 0;

	property = filterparam_get_property(fparam, "#quality");
	gtk_label_set_text(GTK_LABEL(ie->fi_plabel[2]), property);

	property = filterparam_get_property(fparam, "#channels");
	if(property)
		sscanf(property, "%d", &(ie->chancnt));
	gtk_label_set_text(GTK_LABEL(ie->fi_plabel[3]), property);

	property = filterparam_get_property(fparam, "#compression");
	if (property) 
		gtk_label_set_text(GTK_LABEL(ie->fi_plabel[5]), "nan");

	property = filterparam_get_property(fparam, "#framecount");
	if(property) {
		sscanf(property, "%d", &frames);
		ie->frames = frames;
	}
	else
		frames = 0;
	
	if (rate) {
		seconds = frames/rate;
		minutes = seconds / 60;
		seconds -= minutes * 60;
		sprintf(buffer, "%d:%02d min", minutes, seconds);
	} else
		sprintf(buffer, "nan");

	gtk_label_set_text(GTK_LABEL(ie->fi_plabel[4]), buffer);
	sprintf(buffer, "%f", ie->maxrms);
	gtk_label_set_text(GTK_LABEL(ie->fi_plabel[6]), buffer);
}

static void ie_stats_cb(GtkWidget *bla, struct imp_s *ie)
{
	filter_t **ssp, **maxrms, *readfile;
	filter_param_t *param;
	GtkWidget *ed;
	float rms, mrms, percentage;
	int i;
	long bsize=1;
	char buffer[128];

	if(ie->gotfile==0) {
		ed=gnome_error_dialog("Select a file first!");
		gnome_dialog_set_parent(GNOME_DIALOG(ed), GTK_WINDOW(ie->dialog));
		gnome_dialog_run_and_close(GNOME_DIALOG(ed));
		return;
	}
	if (ie->gotfile==2)
		return;

	gtk_widget_set_sensitive(bla, FALSE);

	ssp = ALLOCN(ie->chancnt, filter_t*);
	maxrms = ALLOCN(ie->chancnt, filter_t*);

	ie->net = filter_creat(NULL);
	ie->importing = 1;

	readfile = net_add_plugin_by_name(ie->net, "read_file");
	filterparam_set(filterparamdb_get_param(filter_paramdb(readfile), "filename"), 
			&(ie->filename));

	DPRINTF("setup %d channels\n", ie->chancnt);

	for(i=0; i<ie->chancnt; i++) {
		ssp[i] = net_add_plugin_by_name(ie->net, "ssp_streamer");
		if(ssp[i]==NULL) {
			gnome_error_dialog("Couldn't create ssp plugin");
			goto _ie_stats_cleanup;
		}
			
		filterparam_set(filterparamdb_get_param(filter_paramdb(ssp[i]), "bsize"), &bsize);
		maxrms[i] = net_add_plugin_by_name(ie->net, "maxrms");
		if(maxrms[i]==NULL) {
			gnome_error_dialog("Couldn't create maxrms plugin");
			goto _ie_stats_cleanup;
		}

		if (!filterport_connect(filterportdb_get_port(filter_portdb(readfile), PORTNAME_OUT),
                                        filterportdb_get_port(filter_portdb(ssp[i]), PORTNAME_IN)))
			goto _ie_stats_cleanup;

		if (!filterport_connect(filterportdb_get_port(filter_portdb(ssp[i]), PORTNAME_OUT),
                                        filterportdb_get_port(filter_portdb(maxrms[i]), PORTNAME_IN)))
			goto _ie_stats_cleanup;
		
	}
	param = filterparamdb_get_param(filter_paramdb(readfile),
					FILTERPARAM_LABEL_POS);
	gnome_appbar_set_status(GNOME_APPBAR(ie->appbar), _("Analyzing..."));
	
	ie->cancelled=0;

	if (!(ie->context = filter_launch(ie->net, GLAME_BULK_BUFSIZE)) ||
	    (filter_start(ie->context) == -1))
		goto _ie_stats_cleanup;
	
	DPRINTF("Start network for %d samples\n",ie->frames);

	while(!filter_is_ready(ie->context)) {
		while (gtk_events_pending())
			gtk_main_iteration();
		
		usleep(40000);
		percentage = (float)filterparam_val_long(param)/(float)ie->frames;
		if(percentage>1.0)
			percentage = 1.0;
		gnome_appbar_set_progress_percentage(GNOME_APPBAR(ie->appbar),
					  percentage);
	}

	if (ie->cancelled==0) {
		ie->frames = filterparam_val_long(param);
		sprintf(buffer, "%d", ie->frames);
		filterparam_set_property(filterparamdb_get_param(filter_paramdb(ie->readfile), "filename"),
					 "#framecount", 
					 buffer);
		ie->gotstats = 1;


		gnome_appbar_set_status(GNOME_APPBAR(ie->appbar), _("Done."));
		gnome_appbar_set_progress_percentage(GNOME_APPBAR(ie->appbar), 0.0);
		
		mrms = 0.0;
		
		for(i=0; i<ie->chancnt; i++) {
			param = filterparamdb_get_param(filter_paramdb(maxrms[i]),
						"maxrms");
			rms = filterparam_val_double(param);
			if(rms>mrms)
				mrms = rms;
		}

		ie->maxrms = mrms;

		sprintf(buffer, "%f", mrms);
		gtk_label_set_text(GTK_LABEL(ie->fi_plabel[5]), buffer);

		ie_update_plabels(ie);
	} else if (ie->destroyed!=1) {
		gnome_appbar_set_status(GNOME_APPBAR(ie->appbar), _("Cancelled."));
		gnome_appbar_set_progress_percentage(GNOME_APPBAR(ie->appbar), 0.0);
	}
	filter_launchcontext_unref(&ie->context);
	
 _ie_stats_cleanup:
	DPRINTF("Cleanup");
	free(ssp);
	free(maxrms);
	ie->importing=0;
	ie->cancelled=0;
	filter_delete(ie->net);
	gtk_widget_set_sensitive(bla, TRUE);
	if (ie->destroyed==1)
		ie_import_cleanup(ie);
}


static void ie_filename_cb(GtkEditable *edit, struct imp_s *ie)
{
	filter_param_t *fparam;
	char *mimetype;
	int i;

	if (ie->filename)
		g_free(ie->filename);
	ie->filename = gtk_editable_get_chars(edit, 0, -1);

	/* Basic checks. */
	mimetype = gnome_vfs_get_mime_type(ie->filename);
	if (!mimetype) {
		DPRINTF("Cannot get mimetype for %s\n", ie->filename);
		ie->gotfile = 0;
		return;
	}
	gtk_label_set_text(GTK_LABEL(ie->fi_plabel[0]), mimetype);
	DPRINTF("Got file %s with mimetype %s\n", ie->filename, mimetype);

	/* check if its an mp3 */
	if (strcmp(mimetype, "audio/x-mp3") == 0) {
		ie->gotfile = 2;
		ie->gotstats = 0;
		for(i=1; i<MAX_PROPS; i++)
			gtk_label_set_text(GTK_LABEL(ie->fi_plabel[i]), "-");
		gtk_widget_set_sensitive(ie->checkresample, FALSE);
		gtk_widget_set_sensitive(ie->rateentry, FALSE);
		gtk_widget_set_sensitive(ie->getstats, FALSE);
		g_free(mimetype);
		return;
	}
	/* check if its an mp3 */
	if (strcmp(mimetype, "audio/x-ogg") == 0
	    || strcmp(mimetype, "application/x-ogg") == 0) {
		ie->gotfile = 3;
		ie->gotstats = 0;
		for(i=1; i<MAX_PROPS; i++)
			gtk_label_set_text(GTK_LABEL(ie->fi_plabel[i]), "-");
		gtk_widget_set_sensitive(ie->checkresample, FALSE);
		gtk_widget_set_sensitive(ie->rateentry, FALSE);
		gtk_widget_set_sensitive(ie->getstats, FALSE);
		g_free(mimetype);
		return;
	}

	if (strncmp(mimetype, "audio/", 6) != 0) {
		for(i=1; i<MAX_PROPS; i++)
			gtk_label_set_text(GTK_LABEL(ie->fi_plabel[i]), "-");
		gtk_widget_set_sensitive(ie->checkresample, FALSE);
		gtk_widget_set_sensitive(ie->rateentry, FALSE);
		gtk_widget_set_sensitive(ie->getstats, FALSE);
		g_free(mimetype);
		return;
	}
	g_free(mimetype);

	fparam = filterparamdb_get_param(filter_paramdb(ie->readfile), "filename");
	filterparam_set(fparam, &(ie->filename));

	ie_update_plabels(ie);
	gtk_widget_set_sensitive(ie->checkresample, TRUE);
	gtk_widget_set_sensitive(ie->rateentry, TRUE);
	gtk_widget_set_sensitive(ie->getstats, TRUE);
	ie->gotfile = 1;
	ie->gotstats = 0;
}

gpsm_item_t *glame_import_dialog(GtkWindow *parent)
{
	struct imp_s *ie;
	plugin_t        *rf;
	GtkWidget *dialog_vbox2;
	GtkWidget *vbox2;
	GtkWidget *frame3;
	GtkWidget *fileentry2;
	GtkWidget *combo_entry2;
	GtkWidget *vbox4;
	GtkWidget *frame6;
	GtkWidget *statbutton;
	GtkWidget *vbox3;
	GtkWidget *frame4;
	GtkWidget *hbox4;
	GtkWidget *checkresample;
	GtkWidget *vseparator1;
	GtkWidget *ratelabel;
	GtkObject *rateentry_adj;
	GtkWidget *rateentry;
	GtkWidget *frame5;
	/*GtkWidget *normalizebutton;*/
	GtkWidget *appbar2;
	GtkWidget *dialog_action_area2;
	GtkWidget *importbutton;
	/*	GtkWidget *previewbutton; */
	GtkWidget *cancelbutton;
	GtkWidget *table, *tsep, *tlabel;
	int i;
	gpsm_item_t *item;

	rf = plugin_get("read-file");
	if (rf==NULL)
		return NULL;

	ie = (struct imp_s*)calloc(1,sizeof(struct imp_s));
	ie->item = NULL;
	ie->resample = plugin_get("Resample"); /* scheme plugin */
	ie->readfile = filter_instantiate(rf);
	ie->gotfile = 0;
	ie->dialog = gnome_dialog_new (NULL, NULL);
	ie->importing = 0;
	ie->destroyed = 0;
	ie->filename = NULL;
	ie->net = NULL;
	ie->reallydone = 0;

	gtk_container_set_border_width (GTK_CONTAINER (ie->dialog), 1);
	gtk_window_set_policy (GTK_WINDOW (ie->dialog), FALSE, FALSE, FALSE);
	gnome_dialog_close_hides(GNOME_DIALOG(ie->dialog), FALSE);
	gnome_dialog_set_close(GNOME_DIALOG(ie->dialog), FALSE);
	if (parent)
		gnome_dialog_set_parent(GNOME_DIALOG(ie->dialog), parent);

	dialog_vbox2 = GNOME_DIALOG (ie->dialog)->vbox;
	gtk_widget_show (dialog_vbox2);

	vbox2 = gtk_vbox_new (FALSE, 0);
	gtk_widget_show (vbox2);
	gtk_box_pack_start (GTK_BOX (dialog_vbox2), vbox2, TRUE, TRUE, 0);
	
	frame3 = gtk_frame_new (NULL);
	
	gtk_widget_show (frame3);
	gtk_box_pack_start (GTK_BOX (vbox2), frame3, TRUE, TRUE, 0);
	
	fileentry2 = gnome_file_entry_new ("gpsmop::import::filename", "Import File");
	gnome_file_entry_set_modal(GNOME_FILE_ENTRY(fileentry2), TRUE);
	gtk_widget_show (fileentry2);
	gtk_container_add (GTK_CONTAINER (frame3), fileentry2);
	
	combo_entry2 = gnome_file_entry_gtk_entry (GNOME_FILE_ENTRY (fileentry2));
	gtk_widget_show (combo_entry2);
	
	//	ie->edit = GTK_WIDGET(GTK_EDITABLE(gnome_entry_gtk_entry(
	//					   GNOME_ENTRY(GNOME_FILE_ENTRY(fileentry2)->gentry))));
	ie->edit = GTK_WIDGET(GTK_EDITABLE(gnome_file_entry_gtk_entry(GNOME_FILE_ENTRY(fileentry2))));

	gtk_signal_connect(GTK_OBJECT(ie->edit), "changed",
			   (GtkSignalFunc)ie_filename_cb, ie);

	vbox4 = gtk_vbox_new (FALSE, 0);
	gtk_widget_show (vbox4);
	gtk_box_pack_start (GTK_BOX (dialog_vbox2), vbox4, FALSE, FALSE, 0);
	
	frame6 = gtk_frame_new (_("File Information"));
	gtk_widget_show (frame6);
	gtk_box_pack_start (GTK_BOX (vbox4), frame6, TRUE, TRUE, 0);
	
	table = gtk_table_new (8, 3, FALSE);
	gtk_widget_show (table);
	gtk_container_add (GTK_CONTAINER (frame6), table);
	
	for(i=0; i<MAX_PROPS; i++) {
		tsep = gtk_vseparator_new();
		gtk_widget_show(tsep);
		gtk_table_attach (GTK_TABLE (table), tsep, 1, 2, i, i+1,
				  (GtkAttachOptions) (GTK_FILL),
				  (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 0, 0);
		tlabel = gtk_label_new(fproplabel[i]);
		gtk_widget_show(tlabel);
		gtk_table_attach (GTK_TABLE (table), tlabel, 0, 1, i, i+1,
				  (GtkAttachOptions) (GTK_FILL),
				  (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 0, 0);
		gtk_misc_set_padding (GTK_MISC (tlabel), 20, 0);
		gtk_label_set_justify (GTK_LABEL (tlabel), GTK_JUSTIFY_LEFT);
		gtk_misc_set_alignment (GTK_MISC (tlabel), 0, 0.5);

		ie->fi_plabel[i] = gtk_label_new("-");
		gtk_widget_show(ie->fi_plabel[i]);
		gtk_table_attach (GTK_TABLE (table), ie->fi_plabel[i], 2, 3, i, i+1,
				  (GtkAttachOptions) (GTK_FILL),
				  (GtkAttachOptions) (GTK_EXPAND | GTK_FILL), 0, 0);
		gtk_label_set_justify (GTK_LABEL (ie->fi_plabel[i]), GTK_JUSTIFY_LEFT);
		gtk_misc_set_padding (GTK_MISC (ie->fi_plabel[i]), 20, 0);
		gtk_misc_set_alignment (GTK_MISC (ie->fi_plabel[i]), 0, 0.5);
	}
		
	ie->getstats = statbutton = gtk_button_new_with_label (_("Get RMS & DC-Offset"));
	gtk_signal_connect(GTK_OBJECT(statbutton), "clicked",
				   (GtkSignalFunc)ie_stats_cb, ie);

	gtk_widget_show (statbutton);
	gtk_box_pack_start (GTK_BOX (vbox4), statbutton, FALSE, FALSE, 0);
	gtk_container_set_border_width (GTK_CONTAINER (statbutton), 3);

	vbox3 = gtk_vbox_new (FALSE, 0);
	gtk_widget_show (vbox3);
	gtk_box_pack_start (GTK_BOX (dialog_vbox2), vbox3, TRUE, TRUE, 0);

	frame4 = gtk_frame_new (NULL);
	gtk_widget_show (frame4);
	gtk_box_pack_start (GTK_BOX (vbox3), frame4, TRUE, TRUE, 0);

	hbox4 = gtk_hbox_new (FALSE, 5);
	gtk_widget_show (hbox4);
	gtk_container_add (GTK_CONTAINER (frame4), hbox4);

	/* Disable resample if there's no plugin */
	if(ie->resample==NULL)
		gtk_widget_set_sensitive(hbox4, FALSE);

	ie->checkresample = checkresample = gtk_check_button_new_with_label (_("Resample"));
	gtk_widget_show (checkresample);
	gtk_box_pack_start (GTK_BOX (hbox4), checkresample, FALSE, FALSE, 0);

	vseparator1 = gtk_vseparator_new ();
	gtk_widget_show (vseparator1);
	gtk_box_pack_start (GTK_BOX (hbox4), vseparator1, FALSE, FALSE, 0);

	ratelabel = gtk_label_new (_("Rate"));
	gtk_widget_show (ratelabel);
	gtk_box_pack_start (GTK_BOX (hbox4), ratelabel, FALSE, FALSE, 0);

	rateentry_adj = gtk_adjustment_new (44100, 0, 96000, 10, 100, 1000);
	ie->rateentry = rateentry = gtk_spin_button_new (GTK_ADJUSTMENT (rateentry_adj), 1, 0);
	gtk_widget_show (rateentry);
	gtk_box_pack_start (GTK_BOX (hbox4), rateentry, TRUE, TRUE, 0);
	
	frame5 = gtk_frame_new (NULL);
	gtk_widget_show (frame5);
	gtk_box_pack_start (GTK_BOX (vbox3), frame5, TRUE, TRUE, 0);

	/*
	normalizebutton = gtk_check_button_new_with_label (_("Normalize"));
	gtk_widget_show (normalizebutton);
	gtk_container_add (GTK_CONTAINER (frame5), normalizebutton);
*/
	ie->appbar = appbar2 = gnome_appbar_new (TRUE, TRUE, GNOME_PREFERENCES_NEVER);
	gtk_widget_show (appbar2);
	gtk_box_pack_start (GTK_BOX (dialog_vbox2), appbar2, TRUE, TRUE, 0);

	dialog_action_area2 = GNOME_DIALOG (ie->dialog)->action_area;
	gtk_button_box_set_layout (GTK_BUTTON_BOX (dialog_action_area2), GTK_BUTTONBOX_EDGE);
	gtk_button_box_set_child_size (GTK_BUTTON_BOX (dialog_action_area2), 150, 30);
	
	gnome_dialog_append_button_with_pixmap (GNOME_DIALOG (ie->dialog),
						_("Import"), GNOME_STOCK_PIXMAP_OPEN);
	importbutton = GTK_WIDGET (g_list_last (GNOME_DIALOG (ie->dialog)->buttons)->data);
	gtk_widget_show (importbutton);
	GTK_WIDGET_SET_FLAGS (importbutton, GTK_CAN_DEFAULT);

/*
	gnome_dialog_append_button_with_pixmap (GNOME_DIALOG (ie->dialog),
						_("Preview"), GNOME_STOCK_PIXMAP_VOLUME);
	previewbutton = GTK_WIDGET (g_list_last (GNOME_DIALOG (ie->dialog)->buttons)->data);
	gtk_widget_show (previewbutton);
	GTK_WIDGET_SET_FLAGS (previewbutton, GTK_CAN_DEFAULT);
*/

	gnome_dialog_append_button (GNOME_DIALOG (ie->dialog), GNOME_STOCK_BUTTON_CANCEL);
	cancelbutton = GTK_WIDGET (g_list_last (GNOME_DIALOG (ie->dialog)->buttons)->data);

	gtk_widget_show (cancelbutton);
	GTK_WIDGET_SET_FLAGS (cancelbutton, GTK_CAN_DEFAULT);

	gnome_dialog_append_button(GNOME_DIALOG(ie->dialog), GNOME_STOCK_BUTTON_HELP);

	gnome_dialog_set_default(GNOME_DIALOG(ie->dialog), IMPORT);
	gnome_dialog_set_sensitive(GNOME_DIALOG(ie->dialog), IMPORT, TRUE);
/*	gnome_dialog_set_sensitive(GNOME_DIALOG(ie->dialog), PREVIEW, TRUE);*/
	gnome_dialog_set_sensitive(GNOME_DIALOG(ie->dialog), CANCEL, TRUE);
	gnome_dialog_button_connect(GNOME_DIALOG(ie->dialog), IMPORT,
				    GTK_SIGNAL_FUNC(ie_import_cb), ie);
/*	gnome_dialog_button_connect(GNOME_DIALOG(ie->dialog), PREVIEW,
				    GTK_SIGNAL_FUNC(ie_preview_cb), ie); */
	gnome_dialog_button_connect(GNOME_DIALOG(ie->dialog), CANCEL, 
				    GTK_SIGNAL_FUNC(ie_cancel_cb), ie);
	gnome_dialog_button_connect(GNOME_DIALOG(ie->dialog), HELP,
				    GTK_SIGNAL_FUNC(glame_help_cb), "The_Import_Dialog");

	/* in case we have a non gnome compliant wm */
	gtk_signal_connect(GTK_OBJECT(ie->dialog),
			   "delete-event",
			   GTK_SIGNAL_FUNC(ie_windowkilled), ie);

	/* run the dialog, return the resulting gpsm item/group */
	do {
		gnome_dialog_run(GNOME_DIALOG(ie->dialog));
	} while (ie->reallydone==0);

	item = ie->item;
	free(ie);

	return item;
}



/*
 * Export.
 */


struct exp_s {
	GtkWidget *dialog, *otypemenu, *ocompmenu;
	GtkWidget *cancelbutton, *appbar;
	int typecnt, comptypes;
	int *indices, *comparray;
	GtkWidget *rbutton[MAX_SFLABEL];
	GtkWidget *renderbutton[MAX_RLABEL];
	gpsm_item_t *item;
	gchar *filename;
	long filetype, compression;
	int running;
	filter_t *net;
	filter_launchcontext_t *context;
};

static long export_default_filetype = -1; /* auto */
static long export_default_compression = AF_COMPRESSION_NONE;
static int export_default_render_option = 0;
static int export_default_sample_option = 0;

static gint ie_comp_menu_cb(GtkMenu *menu, struct exp_s *exp)
{
	exp->compression = glame_menu_get_active_index(menu);
	DPRINTF("Compression Type %li chosen\n", exp->compression);

	return TRUE;
}

static void make_comp_menu(struct exp_s *ie, int ftype)
{
	int i, sformtypes;
	int *sformarray;

	gchar *complabel;
	GtkWidget *menuitem, *compmenu;

	compmenu = gtk_option_menu_get_menu(GTK_OPTION_MENU(ie->ocompmenu));
	if (compmenu)
		gtk_widget_destroy(compmenu);

	compmenu = gtk_menu_new();
	
	menuitem=gtk_menu_item_new_with_label("none");
	gtk_widget_show(menuitem);
	gtk_menu_append(GTK_MENU(compmenu), menuitem);

	if (ftype == -1) { /* auto filetype */
		gtk_widget_set_sensitive(ie->ocompmenu, FALSE);
		goto done;
	}

	sformtypes = afQueryLong(AF_QUERYTYPE_FILEFMT, AF_QUERY_SAMPLE_FORMATS, AF_QUERY_VALUE_COUNT, ftype, 0);
	DPRINTF("%d sample formats\n", sformtypes);
	sformarray = afQueryPointer(AF_QUERYTYPE_FILEFMT, AF_QUERY_SAMPLE_FORMATS, AF_QUERY_VALUES, ftype, 0);
	for(i=0; i<sformtypes;i ++) 
		DPRINTF("%d\n", sformarray[i]);
	
	ie->comptypes = afQueryLong(AF_QUERYTYPE_FILEFMT, AF_QUERY_COMPRESSION_TYPES, AF_QUERY_VALUE_COUNT, ftype, 0);
	DPRINTF("%d compression codecs\n", ie->comptypes);
	if (ie->comptypes>0) {
		ie->comparray = afQueryPointer(AF_QUERYTYPE_FILEFMT, AF_QUERY_COMPRESSION_TYPES, AF_QUERY_VALUES, ftype, 0);
		for(i=0; i<ie->comptypes;i++) {
			char numbuf[16];
			if (!(complabel = (char*)afQueryPointer(AF_QUERYTYPE_COMPRESSION, AF_QUERY_LABEL, ie->comparray[i], 0 ,0))) {
				snprintf(numbuf, 15, "%i", ie->comparray[i]);
				complabel = numbuf;
			}
			menuitem = gtk_menu_item_new_with_label(complabel);
			gtk_widget_show(menuitem);
			gtk_menu_append(GTK_MENU(compmenu), menuitem);
		}
		gtk_widget_set_sensitive(ie->ocompmenu, TRUE);
		gtk_signal_connect(GTK_OBJECT(compmenu),
				   "selection_done",
				   (GtkSignalFunc)ie_comp_menu_cb, ie);
	} else
		gtk_widget_set_sensitive(ie->ocompmenu, FALSE);

 done:	
	gtk_option_menu_set_menu(GTK_OPTION_MENU (ie->ocompmenu), compmenu);
	gtk_option_menu_set_history (GTK_OPTION_MENU (ie->ocompmenu), 0);
}


static gint ie_type_menu_cb(GtkMenu *menu, struct exp_s *ie)
{
	int val;

	val = glame_menu_get_active_index(menu);
	if (val>0) {
		make_comp_menu(ie, ie->indices[val-1]);
		ie->filetype=val;
	} else {
		make_comp_menu(ie, -1);
		ie->filetype=-1;
		ie->compression = AF_COMPRESSION_NONE;
	}

	return TRUE;
}

static void export_cleanup(struct exp_s *e)
{
	//gtk_signal_disconnect_by_data(e->cancelbutton, e);
	if (e->running==1) {
		filter_terminate(e->context);
		return;
	}
	if (e->dialog)
		//gnome_dialog_close(GNOME_DIALOG(e->dialog));
		gtk_widget_destroy(e->dialog);

	free(e);
}

static gint exp_windowkilled(GtkWidget *bla,  GdkEventAny *event, gpointer data)
{
	struct exp_s *exp = (struct exp_s*)data;
	DPRINTF("window was killed\n");
	export_cleanup(exp);
	return TRUE;
}


static void exp_cancel_cb(GtkWidget *bla, struct exp_s *e)
{
	DPRINTF("cancel pressed in export window\n");
	export_cleanup(e);
}

static gint export_filename_cb(GtkEditable *edit, struct exp_s *exp)
{
	gchar *filename;

	filename = gtk_editable_get_chars(edit, 0, -1);
	
	if(exp->filename != NULL)
		g_free(exp->filename);
	
	exp->filename = g_strdup(filename);

	g_free(filename);

	return TRUE;
}

static void export_cb(GtkWidget *bla, struct exp_s *exp) 
{
	filter_t *net, *swin, *writefile, *render;
	filter_paramdb_t *db;
	filter_param_t *param;
	filter_port_t *source, *dest;
	filter_pipe_t *pipe;
	gpsm_grp_t *grp;
	gpsm_item_t *it;
	double pos;
	GtkWidget *ed;
	int sfi, ri;
	int totalframes;
	float percentage;

	gtk_widget_set_sensitive(bla, FALSE);

	if(exp->filename==NULL) {
		ed=gnome_error_dialog("Select a filename first!");
		gnome_dialog_set_parent(GNOME_DIALOG(ed), GTK_WINDOW(exp->dialog));
		gnome_dialog_run_and_close(GNOME_DIALOG(ed));
		gtk_widget_set_sensitive(bla, TRUE);
		return;
	}

	/* Build temporary group out of flattened item->item. */
	if (!(grp = gpsm_flatten(exp->item))) {
		gtk_widget_set_sensitive(bla, TRUE);
		return;
	}
	
	for(sfi=0; sfi<MAX_SFLABEL; sfi++)
		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(exp->rbutton[sfi])))
			break;
	for(ri=0; ri<MAX_RLABEL; ri++)
		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(exp->renderbutton[ri])))
			break;
	/* ignore mono/stereo advice, if 1/2 tracks are going to be saved. */
	if (ri == gpsm_grp_nritems(grp))
		ri = 0;

	/* Build basic network. */
	net = filter_creat(NULL);
	writefile = net_add_plugin_by_name(net, "write_file");
	db = filter_paramdb(writefile);

	param = filterparamdb_get_param(db, "filename");
	if (filterparam_set(param, &(exp->filename)) == -1)
		goto fail_cleanup;

	param = filterparamdb_get_param(db, "sampleformat");
	if (filterparam_set(param, &(sf_format[sfi])) == -1)
		goto fail_cleanup;

	param = filterparamdb_get_param(db, "samplewidth");
	if (filterparam_set(param, &(sf_width[sfi])) == -1)
		goto fail_cleanup;

	if (exp->filetype!=-1) {
		param = filterparamdb_get_param(db, "filetype");
		if (filterparam_set(param, &(exp->filetype)) == -1)
			goto fail_cleanup;
	}

	param = filterparamdb_get_param(db, "compression");
	if (filterparam_set(param, &(exp->compression)) == -1)
		goto fail_cleanup;

	dest = filterportdb_get_port(filter_portdb(writefile), PORTNAME_IN); 

	if (ri == 1 || ri == 2) {
		render = net_add_plugin_by_name(net, "render");
		source = filterportdb_get_port(filter_portdb(render), PORTNAME_OUT);
		if (!(pipe = filterport_connect(source, dest)))
			goto fail_cleanup;
		if (ri == 1)
			pos = 0.0;
		else
			pos = -1.57;
		filterparam_set(filterparamdb_get_param(filterpipe_sourceparamdb(pipe), "position"), &pos);
		if (ri == 2) {
			if (!(pipe = filterport_connect(source, dest)))
				goto fail_cleanup;
			pos = 1.57;
			filterparam_set(filterparamdb_get_param(filterpipe_sourceparamdb(pipe), "position"), &pos);
		}
	}

	gpsm_grp_foreach_item(grp, it)
		if (!(swin = net_add_gpsm_input(net, (gpsm_swfile_t *)it,
						0, -1, 0)))
			goto fail_cleanup;

	if (net_apply_node(net, ri == 0 ? writefile : render) == -1)
		goto fail_cleanup;
	
	param = filterparamdb_get_param(filter_paramdb(writefile),
					FILTERPARAM_LABEL_POS);

	gnome_appbar_set_status(GNOME_APPBAR(exp->appbar), _("Exporting..."));

	exp->net = net;

	if (!(exp->context = filter_launch(net, GLAME_BULK_BUFSIZE))
	    || filter_start(exp->context) == -1)
		goto fail_cleanup;

	exp->running=1;
	totalframes = gpsm_item_hsize(grp);

	while(!filter_is_ready(exp->context)) {
		while (gtk_events_pending())
			gtk_main_iteration();
		
		usleep(40000);
		percentage = (float)filterparam_val_long(param)/(float)totalframes;
		if(percentage>1.0)
			percentage = 1.0;
		gnome_appbar_set_progress_percentage(GNOME_APPBAR(exp->appbar),
					  percentage);
	}
	exp->running = 0;

	/* remember new defaults */
	export_default_filetype = exp->filetype;
	export_default_compression = exp->compression;
	export_default_render_option = ri;
	export_default_sample_option = sfi;

	filter_delete(net);
	filter_launchcontext_unref(&exp->context);
	gpsm_item_destroy((gpsm_item_t *)grp);
	export_cleanup(exp);
	return;

 fail_cleanup:
	gnome_appbar_set_status(GNOME_APPBAR(exp->appbar), _("Error!"));
	glame_network_error_dialog(net, "Failed to create exporting network");
	filter_delete(net);
	gpsm_item_destroy((gpsm_item_t *)grp);
	gtk_widget_set_sensitive(bla, TRUE);
}


GnomeDialog *glame_export_dialog(gpsm_item_t *item, GtkWindow *parent) 
{	
	struct exp_s *ie;
	GtkWidget *dialog, *menu, *mitem, *bigbox, *typecompbox, *valbox;
	GtkWidget *dialog_vbox2, *vbox, *frame, *frame2, *frame3, *fentry;
	GtkWidget *framebox, *combo_entry, *frame4, *frame4box, *edit, *dialog_action_area;
	GSList *rbuttons, *renderbuttons;
	int i;
	gchar *suffix;

	/* alloc exp structure. */
	ie = (struct exp_s*)calloc(1,sizeof(struct exp_s));
	ie->item = item;
	ie->filename = NULL;
	ie->filetype = export_default_filetype;
	ie->compression = export_default_compression;
	ie->running = 0;
	/* open new dialog window */
	dialog = ie->dialog = gnome_dialog_new(NULL, NULL);
	gtk_window_set_policy(GTK_WINDOW(dialog), FALSE, FALSE, FALSE);
	gnome_dialog_close_hides(GNOME_DIALOG(dialog), FALSE);
	gnome_dialog_set_close(GNOME_DIALOG(dialog), FALSE);
	if (parent)
		gnome_dialog_set_parent(GNOME_DIALOG(dialog), parent);

	dialog_action_area = GNOME_DIALOG(dialog)->action_area;
	gtk_button_box_set_layout (GTK_BUTTON_BOX (dialog_action_area), GTK_BUTTONBOX_EDGE);
	gtk_button_box_set_child_size (GTK_BUTTON_BOX (dialog_action_area), 150, 30);

	dialog_vbox2 = GNOME_DIALOG (ie->dialog)->vbox;

	vbox = gtk_vbox_new (FALSE, 0);
	gtk_widget_show (vbox);
	gtk_box_pack_start (GTK_BOX (dialog_vbox2), vbox, TRUE, TRUE, 0);

	bigbox = gtk_hbox_new (FALSE, 0);
	gtk_widget_show (bigbox);
	gtk_box_pack_start (GTK_BOX (dialog_vbox2), bigbox, TRUE, TRUE, 0);

	typecompbox = gtk_vbox_new (TRUE, 5);
	gtk_widget_show (typecompbox);
	gtk_box_pack_start (GTK_BOX (bigbox), typecompbox, TRUE, TRUE, 0);

	fentry = gnome_file_entry_new ("gpsmop::export::filename", "Export File");
	gnome_file_entry_set_modal(GNOME_FILE_ENTRY(fentry), TRUE);
	gtk_widget_show(fentry);
	gtk_box_pack_start (GTK_BOX (vbox), fentry, TRUE, TRUE, 0);

	combo_entry = gnome_file_entry_gtk_entry (GNOME_FILE_ENTRY (fentry));
	gtk_widget_show(combo_entry);

	//	edit = GTK_WIDGET(GTK_EDITABLE(gnome_entry_gtk_entry(GNOME_ENTRY(GNOME_FILE_ENTRY(fentry)->gentry))));
	edit = GTK_WIDGET(GTK_EDITABLE(gnome_file_entry_gtk_entry(GNOME_FILE_ENTRY(fentry))));
	
	gtk_signal_connect(GTK_OBJECT(edit), "changed",
			   (GtkSignalFunc)export_filename_cb, ie);

	frame = gtk_frame_new("File Format");
	gtk_widget_show(frame);
	gtk_box_pack_start (GTK_BOX (typecompbox), frame, TRUE, TRUE, 0);
	framebox =  gtk_hbox_new(FALSE, 0);
	gtk_widget_show(framebox);
	gtk_container_add(GTK_CONTAINER(frame), framebox);

	frame2 = gtk_frame_new("Compression Type");
	gtk_widget_show(frame2);
	gtk_box_pack_start (GTK_BOX (typecompbox), frame2, TRUE, TRUE, 0);

	frame4 = gtk_frame_new("Render Options");
	gtk_widget_show(frame4);
	gtk_box_pack_start (GTK_BOX (typecompbox), frame4, TRUE, TRUE, 0);
	frame4box =  gtk_hbox_new(FALSE, 0);
	gtk_widget_show(frame4box);
	gtk_container_add(GTK_CONTAINER(frame4), frame4box);
	
	ie->appbar = gnome_appbar_new (TRUE, TRUE, GNOME_PREFERENCES_NEVER);
	gtk_widget_show (ie->appbar);
	gtk_box_pack_start (GTK_BOX (dialog_vbox2), ie->appbar, TRUE, TRUE, 0);

	renderbuttons = NULL;
	for(i=0; i<MAX_RLABEL; i++) {
		ie->renderbutton[i]=gtk_radio_button_new_with_label(renderbuttons, rlabel[i]);
		renderbuttons=gtk_radio_button_group(GTK_RADIO_BUTTON(ie->renderbutton[i]));
		if (i == export_default_render_option)
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(ie->renderbutton[i]), TRUE);
		gtk_widget_show(ie->renderbutton[i]);
		gtk_box_pack_start(GTK_BOX(frame4box), ie->renderbutton[i], TRUE, TRUE, 0);
	}

	frame3 = gtk_frame_new("Sample Format");
	gtk_widget_show(frame3);
	gtk_box_pack_start (GTK_BOX (bigbox), frame3, TRUE, TRUE, 0);
	valbox = gtk_vbox_new (TRUE, 5);
	gtk_widget_show (valbox);
	gtk_container_add(GTK_CONTAINER(frame3), valbox);
	
	rbuttons = NULL;
	for(i=0; i<MAX_SFLABEL; i++) {
		ie->rbutton[i] = gtk_radio_button_new_with_label(rbuttons, sflabel[i]);
		rbuttons = gtk_radio_button_group(GTK_RADIO_BUTTON(ie->rbutton[i]));
		if (i == export_default_sample_option)
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(ie->rbutton[i]), TRUE);
		gtk_widget_show(ie->rbutton[i]);
		gtk_box_pack_start (GTK_BOX (valbox), ie->rbutton[i], TRUE, TRUE, 0);
	}
	/* now construct option menu with available filetypes */
	ie->typecnt = afQueryLong(AF_QUERYTYPE_FILEFMT, AF_QUERY_ID_COUNT,0 ,0 ,0);

	ie->indices = afQueryPointer(AF_QUERYTYPE_FILEFMT, AF_QUERY_IDS, 0 ,0, 0);

	ie->otypemenu = gtk_option_menu_new ();
	gtk_widget_show(ie->otypemenu);
	gtk_box_pack_start(GTK_BOX(framebox), ie->otypemenu, TRUE, TRUE, 0);
	menu = gtk_menu_new();
	
	DPRINTF("typecnt=%d\n", ie->typecnt);

	mitem =  gtk_menu_item_new_with_label("auto");
	gtk_widget_show(mitem);
	gtk_menu_append (GTK_MENU (menu), mitem);

	for(i=0; i<ie->typecnt; i++)  {
		suffix = (char*)afQueryPointer(AF_QUERYTYPE_FILEFMT, AF_QUERY_LABEL, ie->indices[i] ,0 ,0);
		mitem = gtk_menu_item_new_with_label(suffix);
		gtk_widget_show(mitem);
		gtk_menu_append (GTK_MENU (menu), mitem);
	}
	gtk_option_menu_set_menu (GTK_OPTION_MENU (ie->otypemenu), menu);
	gtk_option_menu_set_history (GTK_OPTION_MENU (ie->otypemenu),
				     ie->filetype == -1 ? 0 : ie->filetype);

	/* compression combo-box, build by simulating a type choose */
	ie->ocompmenu = gtk_option_menu_new ();
	gtk_widget_show(ie->ocompmenu);
	gtk_container_add(GTK_CONTAINER(frame2), ie->ocompmenu);
	ie_type_menu_cb(GTK_MENU(gtk_option_menu_get_menu(GTK_OPTION_MENU(ie->otypemenu))), ie);

	/* filetype is auto, so compression can't be set */
	gtk_widget_set_sensitive(ie->ocompmenu, FALSE);

	gnome_dialog_append_button(GNOME_DIALOG (ie->dialog), _("Export"));
	gnome_dialog_set_default(GNOME_DIALOG(ie->dialog), OK);
	gnome_dialog_append_button(GNOME_DIALOG (ie->dialog), GNOME_STOCK_BUTTON_CANCEL);
	gnome_dialog_append_button(GNOME_DIALOG(ie->dialog), GNOME_STOCK_BUTTON_HELP);
	//ie->cancelbutton = GTK_WIDGET (g_list_last (GNOME_DIALOG (ie->dialog)->buttons)->data);
	
	gnome_dialog_button_connect(GNOME_DIALOG(ie->dialog), OK,
				    GTK_SIGNAL_FUNC(export_cb), ie);

	gnome_dialog_button_connect(GNOME_DIALOG(ie->dialog), CANCEL, 
				    GTK_SIGNAL_FUNC(exp_cancel_cb), ie);
	gnome_dialog_button_connect(GNOME_DIALOG(ie->dialog), HELP,
				    GTK_SIGNAL_FUNC(glame_help_cb), "The_Export_Dialog");

	gtk_signal_connect(GTK_OBJECT(menu),
			   "selection_done",
			   GTK_SIGNAL_FUNC(ie_type_menu_cb), ie);

	/* in case we have a non gnome compliant wm */
	gtk_signal_connect(GTK_OBJECT(ie->dialog),
			   "delete-event",
			   GTK_SIGNAL_FUNC(exp_windowkilled), ie);

	return (GnomeDialog *)dialog;
}

