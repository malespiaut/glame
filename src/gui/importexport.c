/*
 * importexport.c
 * $Id: importexport.c,v 1.51 2005/01/10 23:09:53 nold Exp $
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
	int mad_err;
	int mad_err_cnt;
	int mad_err_at_eof;
	const char *mad_last_err;
	char *mad_buffer;
	int mad_channels;
	swfd_t *mad_swfds;
	int mad_bufsize;
	char *mad_buf;
#endif
	struct timeval start_time;
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
	long ret, pos;
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
	pos = 0;
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
		if (pos/(1152*32) < (pos+ret)/(1152*32)) {
			gtk_progress_bar_pulse(gnome_appbar_get_progress(
				GNOME_APPBAR(ie->appbar)));
			while (gtk_events_pending())
				gtk_main_iteration();
			if (ie->cancelled)
				break;
		}
		pos += ret;
	}

	/* finish gpsm, if no error */
	if (ret == 0 && !ie->cancelled) {
		gpsm_item_t *item;
		gpsm_grp_foreach_item(ie->item, item)
			gpsm_invalidate_swapfile(gpsm_swfile_filename(item));
	} else /* if (ret < 0) */ {
		if (!ie->cancelled)
			gnome_dialog_run_and_close(GNOME_DIALOG(
				gnome_error_dialog(_("Error importing Ogg file."))));
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
	struct imp_s *ie = data;
	ie->mad_err = 1;
	ie->mad_err_at_eof = 1;
	ie->mad_last_err = mad_stream_errorstr(stream);
	if (ie->mad_pos != 0)
		ie->mad_err_cnt++;
	DPRINTF("MAD error pos=%i: %s\n", ie->mad_pos, ie->mad_last_err);
	/* Ignore recoverable errors. */
	if (MAD_RECOVERABLE(stream->error))
		return MAD_FLOW_IGNORE;
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
	int i;

	/* Need to ignore frames with (recovered) error. */
	if (ie->mad_err) {
		ie->mad_err = 0;
		return MAD_FLOW_CONTINUE;
	}
	ie->mad_err_at_eof = 0;

	if (ie->mad_pos == 0) {
		/* alloc gpsm group, etc. */
		ie->item = (gpsm_item_t *)gpsm_newgrp(g_basename(ie->filename));
		ie->mad_swfds = malloc(pcm->channels * sizeof(swfd_t));
		ie->mad_channels = pcm->channels;
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
			ie->mad_swfds[i] = sw_open(gpsm_swfile_filename(swfile), O_RDWR);
		}
		gettimeofday(&ie->start_time, NULL);
	}

	if (!ie->mad_buf
	    || ie->mad_bufsize < pcm->length) {
		if (ie->mad_buf)
			free(ie->mad_buf);
		DPRINTF("mad buffer size %i samples\n", pcm->length);
		ie->mad_buf = malloc(SAMPLE_SIZE*pcm->length);
		ie->mad_bufsize = pcm->length;
	}
	for (i=0; i<pcm->channels; ++i) {
		unsigned int nsamples = pcm->length;
		mad_fixed_t const *data = pcm->samples[i];
		SAMPLE *b = (SAMPLE *)ie->mad_buf;

		while (nsamples--)
		    *b++ = mad_f_todouble(*data++);

		sw_write(ie->mad_swfds[i], ie->mad_buf, SAMPLE_SIZE*pcm->length);
	}

	/* all 32 frames show some progress and check for user input */
	if (ie->mad_pos/(1152*32) < (ie->mad_pos+pcm->length)/(1152*32)) {
		struct timeval tv;
		char msg[256];
		gettimeofday(&tv, NULL);
		snprintf(msg, 256, _("Importing... %.3lf ksamples/s"),
			 ((double)(ie->mad_pos+pcm->length)/(tv.tv_sec - ie->start_time.tv_sec + (tv.tv_usec - ie->start_time.tv_usec)/1000000.0)/1000.0));
		// for gnuplot graph
		printf("%.3lf\t%li\n",
		       (tv.tv_sec - ie->start_time.tv_sec + (tv.tv_usec - ie->start_time.tv_usec)/1000000.0),
		       (long)(ie->mad_pos+pcm->length));
		gnome_appbar_set_status(GNOME_APPBAR(ie->appbar), msg);
		gtk_progress_bar_pulse(gnome_appbar_get_progress(
			GNOME_APPBAR(ie->appbar)));
		while (gtk_events_pending())
			gtk_main_iteration();
		if (ie->cancelled)
			return MAD_FLOW_BREAK;
	}
	ie->mad_pos += pcm->length;

	return MAD_FLOW_CONTINUE;
}
static void ie_import_mp3(struct imp_s *ie)
{
	int fd, i;
	struct stat s;
	struct mad_decoder decoder;
	int result;

	fd = open(ie->filename, O_RDONLY);
	if (fd == -1)
		return;
	fstat(fd, &s);
	ie->mad_err = 0;
	ie->mad_err_at_eof = 0;
	ie->mad_err_cnt = 0;
	ie->mad_length = s.st_size;
	ie->mad_last_err = NULL;
	ie->mad_buffer = mmap(NULL, s.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
	if (ie->mad_buffer == MAP_FAILED) {
		close(fd);
		return;
	}
	ie->mad_buf = NULL;
	ie->mad_bufsize = 0;

	mad_decoder_init(&decoder, ie,
			 ie_import_mp3_input, 0, 0,
			 ie_import_mp3_output,
			 ie_import_mp3_error, 0);
	ie->mad_pos = 0;
	ie->importing = 1;
	gnome_appbar_set_status(GNOME_APPBAR(ie->appbar), _("Importing..."));

	result = mad_decoder_run(&decoder, MAD_DECODER_MODE_SYNC);
	mad_decoder_finish(&decoder);

	for (i=0; i<ie->mad_channels; ++i)
		sw_close(ie->mad_swfds[i]);
	if (ie->mad_buf)
		free(ie->mad_buf);

	munmap(ie->mad_buffer, s.st_size);
	close(fd);

	if (result == 0) {
		gpsm_item_t *item;
		gpsm_grp_foreach_item(ie->item, item)
			gpsm_invalidate_swapfile(gpsm_swfile_filename(item));
		if (ie->mad_err_cnt - ie->mad_err_at_eof > 0) {
			char errmsg[1024];
			snprintf(errmsg, 1024, _("Had %i recoverable errors, last was: %s"),
				 ie->mad_err_cnt, ie->mad_last_err);
			gnome_dialog_run_and_close(GNOME_DIALOG(
				gnome_warning_dialog(errmsg)));
		}
	} else {
		if (!ie->cancelled) {
			char errmsg[1024];
			snprintf(errmsg, 1024, _("Failed importing mp3 file: %s"),
				 ie->mad_last_err);
			gnome_dialog_run_and_close(GNOME_DIALOG(
				gnome_error_dialog(errmsg)));
		}
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

static int ie_update_plabels(struct imp_s *ie)
{
	filter_param_t *fparam;
	gchar *property;
	unsigned int frames, minutes, seconds;
	int rate, ftype, version;
	gchar buffer[255];

	fparam = filterparamdb_get_param(filter_paramdb(ie->readfile), "filename");
	property = filterparam_get_property(fparam, "#format");
	if (property) {
		const char *label;
		sscanf(property, "%d %d", &ftype, &version);
		DPRINTF("format property: %s, ftype=%d, version=%d\n",
			property, ftype, version);
		label = afQueryPointer(AF_QUERYTYPE_FILEFMT, AF_QUERY_LABEL,
				       ftype,0 ,0);
		sprintf(buffer, "%s format", label ? label : "unknown");
	} else
		return -1;
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

	return 0;
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
	if (access(ie->filename, R_OK) == -1)
		return;

	/* Basic checks. */
	mimetype = gnome_vfs_get_mime_type(ie->filename);
	if (!mimetype) {
		DPRINTF("Cannot get mimetype for %s\n", ie->filename);
		ie->gotfile = 0;
		ie->gotstats = 0;
		gtk_label_set_text(GTK_LABEL(ie->fi_plabel[0]), "unknown");
		for(i=1; i<MAX_PROPS; i++)
			gtk_label_set_text(GTK_LABEL(ie->fi_plabel[i]), "-");
		gtk_widget_set_sensitive(ie->checkresample, FALSE);
		gtk_widget_set_sensitive(ie->rateentry, FALSE);
		gtk_widget_set_sensitive(ie->getstats, FALSE);
		return;
	}
	gtk_label_set_text(GTK_LABEL(ie->fi_plabel[0]), mimetype);
	DPRINTF("Got file %s with mimetype %s\n", ie->filename, mimetype);

	/* check if its an mp3 */
	if (strcmp(mimetype, "audio/x-mp3") == 0
	    || strcmp(mimetype, "audio/mpeg") == 0) {
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

	/* check if its an ogg */
	if (strcmp(mimetype, "audio/x-ogg") == 0
	    || strcmp(mimetype, "application/x-ogg") == 0
	    || strcmp(mimetype, "application/ogg") == 0) {
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
	g_free(mimetype);

	/* check if it's not recognized by the audio_in plugin */
	fparam = filterparamdb_get_param(filter_paramdb(ie->readfile), "filename");
	filterparam_set(fparam, &(ie->filename));
	ie_update_plabels(ie);
	if (ie_update_plabels(ie) == -1) {
		ie->gotfile = 0;
		ie->gotstats = 0;
		for(i=1; i<MAX_PROPS; i++)
			gtk_label_set_text(GTK_LABEL(ie->fi_plabel[i]), "-");
		gtk_widget_set_sensitive(ie->checkresample, FALSE);
		gtk_widget_set_sensitive(ie->rateentry, FALSE);
		gtk_widget_set_sensitive(ie->getstats, FALSE);
		return;
	}

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
	//gtk_window_set_policy (GTK_WINDOW (ie->dialog), FALSE, FALSE, FALSE);
	gtk_window_set_resizable (GTK_WINDOW(ie->dialog),FALSE);
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

	g_signal_connect(GTK_OBJECT(ie->edit), "changed",
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
	g_signal_connect(GTK_OBJECT(statbutton), "clicked",
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

	gnome_dialog_append_button (GNOME_DIALOG (ie->dialog), GTK_STOCK_CANCEL);
	cancelbutton = GTK_WIDGET (g_list_last (GNOME_DIALOG (ie->dialog)->buttons)->data);

	gtk_widget_show (cancelbutton);
	GTK_WIDGET_SET_FLAGS (cancelbutton, GTK_CAN_DEFAULT);

	gnome_dialog_append_button(GNOME_DIALOG(ie->dialog), GTK_STOCK_HELP);

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
	g_signal_connect(GTK_OBJECT(ie->dialog),
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
	GtkWidget *dialog, *notebook, *ocomp_combo_box, *filechooser;
	GtkWidget *otype_combo_box;
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
        GtkWidget *quality_select, *mode_select, *bitrate_select;
        GtkWidget *title, *artist, *album, *year, *comment, *track, *genre;
	int mp3_menu_index, mp3tab_num ;
        GtkWidget *quality_select_ogg, *mode_select_ogg, *bitrate_select_ogg;
        GtkWidget *title_ogg, *artist_ogg, *album_ogg, *year_ogg, *comment_ogg, *track_ogg, *genre_ogg;
	int ogg_menu_index, oggvorbistab_num;
};

static long export_default_filetype = -1; /* auto */
static long export_default_compression = AF_COMPRESSION_NONE;
static int export_default_render_option = 0;
static int export_default_sample_option = 0;

static gint ie_comp_menu_cb(GtkMenu *menu, struct exp_s *exp)
{
	exp->compression = gtk_combo_box_get_active (GTK_COMBO_BOX(exp->ocomp_combo_box));
	// exp->compression = glame_menu_get_active_index(menu);
	DPRINTF("Compression Type %li chosen\n", exp->compression);

	return TRUE;
}

static void make_comp_menu(struct exp_s *ie, int ftype)
{
	int i, sformtypes;
	int *sformarray;
	gchar *complabel;

	if (ftype == -1) { /* auto filetype */
		gtk_widget_set_sensitive(ie->ocomp_combo_box, FALSE);
		return;
	}

	for(i=ie->comptypes+1; i>=0; i--){
		DPRINTF("remove index %i\n",i);
		gtk_combo_box_remove_text ( GTK_COMBO_BOX (ie->ocomp_combo_box),i);
	}

	gtk_combo_box_append_text(GTK_COMBO_BOX (ie->ocomp_combo_box),"none");

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
			gtk_combo_box_append_text(GTK_COMBO_BOX (ie->ocomp_combo_box),complabel);
	 		gtk_combo_box_set_active (GTK_COMBO_BOX (ie->ocomp_combo_box), 0);
		}
		gtk_widget_set_sensitive(ie->ocomp_combo_box, TRUE);
	} else
		gtk_widget_set_sensitive(ie->ocomp_combo_box, FALSE);
}


void set_export_filename (struct exp_s *ie, gchar *ext)
{
	char *filename;
	filename = g_strconcat(gpsm_item_label(ie->item), ext,NULL);
	gtk_file_chooser_set_current_name(GTK_FILE_CHOOSER(ie->filechooser),filename);
	if (filename) g_free(filename);
	return ;
}

static gint ie_type_menu_cb(GtkComboBox *menu, struct exp_s *ie)
{
	int val;
	gchar  *str1;

	val = gtk_combo_box_get_active(menu);
	#ifdef HAVE_LIBMP3LAME
	if (val == ie->mp3_menu_index) {
		make_comp_menu(ie, -1);
		ie->filetype = 99;
		set_export_filename(ie, ".mp3");
		gtk_notebook_set_current_page(GTK_NOTEBOOK(ie->notebook),ie->mp3tab_num );
		return TRUE;
			} else 
#endif
#ifdef HAVE_LIBVORBISFILE
	if (val == ie->ogg_menu_index) {
		make_comp_menu(ie, -1);
		ie->filetype = 100;
		set_export_filename(ie, ".ogg");
		gtk_notebook_set_current_page(GTK_NOTEBOOK(ie->notebook),ie->oggvorbistab_num );
		return TRUE;
	} else
#endif
		if (val>0) {
		make_comp_menu(ie, ie->indices[val-1]);
		ie->filetype=val;
		
		// file extension 
		str1 = g_strdup ((char*)afQueryPointer(AF_QUERYTYPE_FILEFMT, AF_QUERY_LABEL,(ie->indices[val-1]), 0 , 0));
		// special cases
		if (!g_ascii_strncasecmp(str1, "next",4)) str1 = g_strdup("au"); 
		else if (!g_ascii_strncasecmp(str1, "bicsf",5)) str1 = g_strdup("bicsf"); 
		else if (!g_ascii_strncasecmp(str1, "nist",5)) str1 = g_strdup("nist"); 
		// Others: ext from 3 first label letters
		else str1 = g_strndup (str1,3);
		str1 = g_strconcat(".",str1,NULL);
		set_export_filename(ie, str1);
		gtk_notebook_set_current_page(GTK_NOTEBOOK(ie->notebook),0);

		if (str1) g_free(str1);
		
	} else {
		make_comp_menu(ie, -1);
		ie->filetype=-1;
		ie->compression = AF_COMPRESSION_NONE;
		gtk_file_chooser_set_current_name(GTK_FILE_CHOOSER(ie->filechooser) ,gpsm_item_label(ie->item));
		gtk_notebook_set_current_page(GTK_NOTEBOOK(ie->notebook),0);
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

/* static gint export_filename_cb(GtkFileChooser *filechooser, struct exp_s *exp)
{
	gchar *filename;

	filename = gtk_file_chooser_get_filename(filechooser);
puts(	filename );
	if(exp->filename != NULL)
		g_free(exp->filename);
	
	exp->filename = g_strdup(filename);

	g_free(filename);

	return TRUE;
}
*/

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
	gchar *output_plugin = "write_file";
	const gchar *string = NULL;
	// gchar *string2 = NULL;
	int  index;
	
	gtk_widget_set_sensitive(bla, FALSE);
	
	g_free(exp->filename);
	exp->filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(exp->filechooser));
	
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
	
	/* Check if mp3 file type is selected, or if there is .mp3 in filename
	  (can't use mimetype because file does not exist yet) */
	if (exp->filetype == 99
	    || (exp->filetype == -1
		&& strrchr(exp->filename, '.')
		&& strcasecmp(strrchr(exp->filename, '.'), ".mp3") == 0)) {
		output_plugin = "write_mp3_file";
#ifndef HAVE_LIBMP3LAME
		ed = gnome_error_dialog(_("Sorry, no mp3 encoding support.\n"
					  "Install Lame encoder and rebuild Glame."));
		gnome_dialog_set_parent(GNOME_DIALOG(ed), GTK_WINDOW(exp->dialog));
		gnome_dialog_run_and_close(GNOME_DIALOG(ed));
		gpsm_item_destroy((gpsm_item_t *)grp);
		return;
#endif
	}

	/* Check if oggvorbis file type is selected, or if there is .ogg in filename
	 * (can't use mimetype because file does not exist yet) */
	if (exp->filetype == 100
	    || (exp->filetype == -1
		&& strrchr(exp->filename, '.')
		&& strcasecmp(strrchr(exp->filename, '.'), ".ogg") == 0)) {
		output_plugin = "write_oggvorbis_file";
#ifndef HAVE_LIBVORBISFILE
		ed = gnome_error_dialog(_("Sorry, no OggVorbis encoding support.\n"
					  "Install Ogg and Vorbis libraries rebuild Glame."));
		gnome_dialog_set_parent(GNOME_DIALOG(ed), GTK_WINDOW(exp->dialog));
		gnome_dialog_run_and_close(GNOME_DIALOG(ed));
		gpsm_item_destroy((gpsm_item_t *)grp);
		return;
#endif
	}

	DPRINTF("output plugin is %s \n",output_plugin);

	/* Build basic network. */
	net = filter_creat(NULL);
	writefile = net_add_plugin_by_name(net, output_plugin);
	db = filter_paramdb(writefile);

	param = filterparamdb_get_param(db, "filename");
	if (filterparam_set(param, &(exp->filename)) == -1)
		goto fail_cleanup;
	
	/* main tab */
	if (!strcmp(output_plugin , "write_file")) {
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
	}
	
#ifdef HAVE_LIBMP3LAME
	/* mp3 */
	else if (!strcmp(output_plugin , "write_mp3_file")) {
	  ri = 2; /* set as stereo to connect a render filter, Lame has its own mono/stereo conversion (mode)*/
	  /* Lame quality */
	  index = gtk_combo_box_get_active (GTK_COMBO_BOX(exp->quality_select));
	  if (index == -1) goto fail_cleanup; 
	  param = filterparamdb_get_param(db, "lame encoding quality");
 	  if (filterparam_set_long(param, index) == -1)
	    goto fail_cleanup; 
	  /* Lame mode */
	  index = gtk_combo_box_get_active (GTK_COMBO_BOX(exp->mode_select));
	  param = filterparamdb_get_param(db, "lame mode");
          if (filterparam_set_long(param, index) == -1)
	    goto fail_cleanup;
	  /* Lame bitrate */
	  index = gtk_combo_box_get_active (GTK_COMBO_BOX(exp->bitrate_select));
          param = filterparamdb_get_param(db, "lame encoding bitrate");
	  if (filterparam_set_long(param, index) == -1)
	   goto fail_cleanup;
	  /* mp3 tags */
	  string = gtk_entry_get_text (GTK_ENTRY (exp->title));
	  param = filterparamdb_get_param(db, "Id3tag_Title");
	  filterparam_from_string(param, string);
	  
	  string = gtk_entry_get_text (GTK_ENTRY (exp->artist));
	  param = filterparamdb_get_param(db, "Id3tag_Artist");
	  filterparam_from_string(param, string);
	  
	  string = gtk_entry_get_text (GTK_ENTRY (exp->album));
	  param = filterparamdb_get_param(db, "Id3tag_Album");
	  filterparam_from_string(param, string);

	  string = gtk_entry_get_text (GTK_ENTRY (exp->year));
	  param = filterparamdb_get_param(db, "Id3tag_Year");
	  filterparam_from_string(param, string);

	  string = gtk_entry_get_text (GTK_ENTRY (exp->comment));
	  param = filterparamdb_get_param(db, "Id3tag_Comment");
	  filterparam_from_string(param, string);

	  string = gtk_entry_get_text (GTK_ENTRY (exp->track));
	  param = filterparamdb_get_param(db, "Id3tag_Track");
	  filterparam_from_string(param, string);

	  string = gtk_entry_get_text (GTK_ENTRY(exp->genre));
	  param = filterparamdb_get_param(db, "Id3tag_Genre");
	  filterparam_from_string(param, string);
	}
#endif

#ifdef HAVE_LIBVORBISFILE
	else if (!strcmp(output_plugin , "write_oggvorbis_file")) {
	  	  
	  /* vorbis quality */
	  index = gtk_combo_box_get_active (GTK_COMBO_BOX(exp->quality_select_ogg));
	  param = filterparamdb_get_param(db, "vorbis encoding quality");
	  if (filterparam_set_long(param, index) == -1)
	    goto fail_cleanup; 
	  DPRINTF("Vorbis qual index = %i\n",index);

	  /* render mode (mono or stereo) */
	  index = gtk_combo_box_get_active (GTK_COMBO_BOX(exp->mode_select_ogg));
	  ri = (index==1) ? 1 : 2 ;

	  string = gtk_entry_get_text (GTK_ENTRY (exp->title_ogg));
	  param = filterparamdb_get_param(db, "Title");
	  filterparam_from_string(param, string);

	  string = gtk_entry_get_text (GTK_ENTRY (exp->artist_ogg));
	  param = filterparamdb_get_param(db, "Artist");
	  filterparam_from_string(param, string);
	  
	  string = gtk_entry_get_text (GTK_ENTRY (exp->album_ogg));
	  param = filterparamdb_get_param(db, "Album");
	  filterparam_from_string(param, string);

	  string = gtk_entry_get_text (GTK_ENTRY (exp->year_ogg));
	  param = filterparamdb_get_param(db, "Year");
	  filterparam_from_string(param, string);
	  
	  string = gtk_entry_get_text (GTK_ENTRY (exp->comment_ogg));
	  param = filterparamdb_get_param(db, "Comment");
	  filterparam_from_string(param, string);
	  
	  string = gtk_entry_get_text (GTK_ENTRY (exp->track_ogg));
	  param = filterparamdb_get_param(db, "Track");
	  filterparam_from_string(param, string);
	  
	  string = gtk_entry_get_text (GTK_ENTRY (exp->genre_ogg));
	  param = filterparamdb_get_param(db, "Genre");
	  filterparam_from_string(param, string);
	}
#endif

	dest = filterportdb_get_port(filter_portdb(writefile), PORTNAME_IN); 
	DPRINTF ("ri = %i \n",ri);
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
	/* first check mp3 and ogg special type */
	if ((exp->filetype == 99) || (exp->filetype == 100)) {  export_default_filetype=  (exp->filetype == 99 ? exp->mp3_menu_index  : exp->ogg_menu_index );}
	else {export_default_filetype = exp->filetype;}
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
	GtkWidget *label_tab1, *label_tab2, *label_tab3 ;
	GtkWidget *dialog, *bigbox, *bigbox2 , *bigbox3, *typecompbox, *valbox, *vboxftype;
	GtkWidget *dialog_vbox2, *vbox, *hbox, *frame, *frame2, *frame3,  *fname, *fnamebox;
	GtkWidget *framebox, *frame4, *frame4box,*dialog_action_area; 
	GSList *rbuttons, *renderbuttons;
	int i;
	gchar *suffix, *title = "";

#ifdef HAVE_LIBMP3LAME
	GtkWidget *boxtags,*boxtags2, *frame5, *frame5box,*frame6, *frame6box ,*frame7, *frame7box  ,*frame8, *frame8box,*frame9, *frame9box,*frame10, *frame10box, *frame11, *frame11box, *frame12, *frame12box, *frame13, *frame13box, *frame14, *frame14box, *frame15, *frame15box;
#endif
#ifdef HAVE_LIBVORBISFILE
	GtkWidget *boxtags3,*boxtags4, *frame16, *frame16box,*frame17, *frame17box ,*frame18, *frame18box  ,*frame19, *frame19box,*frame20, *frame20box,*frame21, *frame21box, *frame22, *frame22box, *frame23, *frame23box, *frame24, *frame24box, *frame25, *frame25box,*frame26, *frame26box;
#endif
	char *string[10]={"0 best,slow","1","2 recommended","3","4","5 standard","6","7","8","9 worst"};
	char *string2[4]={"0 stereo","1 joint stereo","2 dual mono","3 mono"};
	char *string3[14]={"32","40","48","56","64","80","96","112","128","160","192","224","256","320"};
	char *string4[2]={"stereo","mono"};
	char *string5[11]={"0 (roughly 64kb/s)","1 (roughly 80kb/s)","2 (roughly 96kb/s)","3 (roughly 112kb/s)","4 (roughly 128kb/s)","5 (roughly 160kb/s)","6 (roughly 192kb/s)","7 (roughly 224kb/s)","8 (roughly 256kb/s)","9 (roughly 320kb/s)","10 (roughly 500kb/s)"};


	/* alloc exp structure. */
	ie = (struct exp_s*)calloc(1,sizeof(struct exp_s));
	ie->item = item;
	ie->filename = NULL;
	ie->filetype = export_default_filetype;
	ie->compression = export_default_compression;
	ie->running = 0;

	/* open new dialog window */
	title = g_strdup_printf ("Export: %s", gpsm_item_label(item));
	dialog = ie->dialog = gnome_dialog_new(title , NULL);
	if (title) free(title);
	//gtk_window_set_policy(GTK_WINDOW(dialog), FALSE, FALSE, FALSE);
	gtk_window_set_resizable (GTK_WINDOW(dialog),TRUE);
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

	hbox = gtk_hbox_new (FALSE, 0);
	gtk_widget_show (hbox);
	gtk_box_pack_start (GTK_BOX (vbox), hbox, TRUE, TRUE, 0);

	bigbox = gtk_hbox_new (FALSE, 0);
	gtk_widget_show (bigbox);
	gtk_box_pack_start (GTK_BOX (dialog_vbox2), bigbox, TRUE, TRUE, 0);

		
	fname = gtk_frame_new(_("Export as:"));
	gtk_widget_show(fname);
	gtk_box_pack_start (GTK_BOX (hbox), fname,TRUE, TRUE, 0);
	fnamebox =  gtk_hbox_new(FALSE, 0);
	gtk_widget_show(fnamebox);
	gtk_container_add(GTK_CONTAINER(fname), fnamebox);
	
	ie->filechooser = gtk_file_chooser_widget_new(GTK_FILE_CHOOSER_ACTION_SAVE);
	gtk_file_chooser_set_current_name(GTK_FILE_CHOOSER(ie->filechooser) ,gpsm_item_label(item));
	gtk_widget_show(ie->filechooser);
	gtk_box_pack_start (GTK_BOX (fnamebox), ie->filechooser, TRUE, TRUE, 0);
	
	/* fentry = gnome_file_entry_new ("gpsmop::export::filename", "Export File");
	gnome_file_entry_set_modal(GNOME_FILE_ENTRY(fentry), TRUE);
	gtk_widget_show(fentry);
	gtk_box_pack_start (GTK_BOX (fnamebox), fentry, TRUE, TRUE, 0); 

	 combo_entry = gnome_file_entry_gtk_entry (GNOME_FILE_ENTRY (fentry));
	gtk_widget_show(combo_entry); */

	/*edit = GTK_WIDGET(GTK_EDITABLE(gnome_file_entry_gtk_entry(GNOME_FILE_ENTRY(fentry)))); */
	
	/*g_signal_connect(GTK_OBJECT(fentry), "changed",
			   (GtkSignalFunc)export_cb, ie); */
	vboxftype = gtk_vbox_new (FALSE, 0); 
	gtk_widget_show(vboxftype);
	gtk_box_pack_start (GTK_BOX (hbox), vboxftype , FALSE, FALSE, 0);
	frame = gtk_frame_new(_("File Format"));
	gtk_widget_show(frame);
	gtk_box_pack_start (GTK_BOX (vboxftype ), frame, FALSE, FALSE, 0);
	framebox =  gtk_hbox_new(FALSE, 0);
	gtk_widget_show(framebox);
	gtk_container_add(GTK_CONTAINER(frame), framebox);
		
	ie->notebook = gtk_notebook_new();
	gtk_widget_show (ie->notebook);
	gtk_box_pack_start (GTK_BOX (vbox), GTK_WIDGET(ie->notebook), TRUE, TRUE, 0);
	
	/*** MAin tab ***/
	label_tab1 = gtk_label_new(_("Main"));
	gtk_widget_show (label_tab1);
	bigbox = gtk_hbox_new (FALSE, 0); 
	gtk_widget_show(bigbox);
	gtk_notebook_append_page(GTK_NOTEBOOK(ie->notebook),bigbox,label_tab1);

	typecompbox = gtk_vbox_new (TRUE, 5);
	gtk_widget_show (typecompbox);
	gtk_box_pack_start (GTK_BOX (bigbox), typecompbox, FALSE, FALSE,0);

	frame2 = gtk_frame_new(_("Compression Type"));
	gtk_widget_show(frame2);
	gtk_box_pack_start (GTK_BOX (typecompbox), frame2, FALSE, FALSE,0);

	/* compression combo-box, build by simulating a type choose */
	ie->ocomp_combo_box = gtk_combo_box_new_text ();
	gtk_widget_show(ie->ocomp_combo_box);
	gtk_container_add(GTK_CONTAINER(frame2),ie->ocomp_combo_box);
	
	frame4 = gtk_frame_new(_("Render Options"));
	gtk_widget_show(frame4);
	gtk_box_pack_start (GTK_BOX (typecompbox), frame4, FALSE, FALSE,0);
	frame4box =  gtk_hbox_new(FALSE, 0);
	gtk_widget_show(frame4box);
	gtk_container_add(GTK_CONTAINER(frame4), frame4box);
	
	ie->appbar = gnome_appbar_new (TRUE, TRUE, GNOME_PREFERENCES_NEVER);
	gtk_widget_show (ie->appbar);
	gtk_box_pack_start (GTK_BOX (dialog_vbox2), ie->appbar, FALSE, FALSE,0);

	renderbuttons = NULL;
	for(i=0; i<MAX_RLABEL; i++) {
		ie->renderbutton[i]=gtk_radio_button_new_with_label(renderbuttons, rlabel[i]);
		renderbuttons=gtk_radio_button_group(GTK_RADIO_BUTTON(ie->renderbutton[i]));
		if (i == export_default_render_option)
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(ie->renderbutton[i]), TRUE);
		gtk_widget_show(ie->renderbutton[i]);
		gtk_box_pack_start(GTK_BOX(frame4box), ie->renderbutton[i], FALSE, FALSE,0);
	}

	frame3 = gtk_frame_new(_("Sample Format"));
	gtk_widget_show(frame3);
	gtk_box_pack_start (GTK_BOX (bigbox), frame3, FALSE, FALSE, 0);
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
		gtk_box_pack_start (GTK_BOX (valbox), ie->rbutton[i], FALSE, FALSE,0);
	}
	
	/* now construct combo box filetypes */
	ie->otype_combo_box = gtk_combo_box_new_text ();
	
	ie->typecnt = afQueryLong(AF_QUERYTYPE_FILEFMT, AF_QUERY_ID_COUNT,0 ,0 ,0);
	ie->indices = afQueryPointer(AF_QUERYTYPE_FILEFMT, AF_QUERY_IDS, 0 ,0, 0);
	gtk_combo_box_append_text(GTK_COMBO_BOX (ie->otype_combo_box), "auto");
	for(i=0; i<ie->typecnt; i++)  {
		suffix = (char*)afQueryPointer(AF_QUERYTYPE_FILEFMT, AF_QUERY_LABEL,
					       ie->indices[i] ,0 ,0);
		gtk_combo_box_append_text(GTK_COMBO_BOX (ie->otype_combo_box), suffix);
	}
#ifdef HAVE_LIBMP3LAME 
	gtk_combo_box_append_text(GTK_COMBO_BOX (ie->otype_combo_box), "mp3");
	ie->mp3_menu_index = ++i; 
#endif 
#ifdef HAVE_LIBVORBISFILE
	gtk_combo_box_append_text(GTK_COMBO_BOX (ie->otype_combo_box),"ogg vorbis");
	ie->ogg_menu_index = ++i;
#endif
	gtk_combo_box_set_active(GTK_COMBO_BOX (ie->otype_combo_box),
				 ie->filetype == -1 ? 0 : ie->filetype);
	ie_type_menu_cb(GTK_COMBO_BOX (ie->otype_combo_box), ie);
	
	gtk_box_pack_start(GTK_BOX(framebox), GTK_WIDGET(ie->otype_combo_box), FALSE, FALSE, 0);
	gtk_widget_show(GTK_WIDGET(ie->otype_combo_box));
	
#ifdef HAVE_LIBMP3LAME	
	/*** Mp3 lame tab ***/
	label_tab2 = gtk_label_new(_("Mp3 (Lame)"));
	gtk_widget_show (label_tab2);
	bigbox2 = gtk_hbox_new (FALSE, 0); 
	gtk_widget_show(bigbox2);
	gtk_notebook_append_page(GTK_NOTEBOOK(ie->notebook), bigbox2 ,label_tab2);
	ie->mp3tab_num = 1;
	frame5 = gtk_frame_new("MP3 Lame settings");
	gtk_widget_show(frame5);
	gtk_box_pack_start (GTK_BOX (bigbox2), frame5, TRUE, TRUE, 0);
	frame5box = gtk_vbox_new (TRUE, 5);
	gtk_widget_show (frame5box);
	gtk_container_add(GTK_CONTAINER(frame5), frame5box);
	
	/* Lame quality */
	frame6 = gtk_frame_new("Quality");
	gtk_widget_show(frame6);
	gtk_box_pack_start (GTK_BOX (frame5box), frame6, TRUE, TRUE, 0);
	frame6box = gtk_vbox_new (TRUE, 6);
	gtk_widget_show (frame6box);
	gtk_container_add(GTK_CONTAINER(frame6), frame6box);
	ie->quality_select = gtk_combo_box_new_text ();
	for (i=0 ;i<10; i++) {
		gtk_combo_box_append_text (GTK_COMBO_BOX (ie->quality_select ),string[i]);
	}
	gtk_combo_box_set_active (GTK_COMBO_BOX (ie->quality_select), 2);
	gtk_box_pack_start (GTK_BOX(frame6box), ie->quality_select, TRUE, TRUE, 0);
	gtk_widget_show (ie->quality_select);
	
	/* Lame mode */
	frame7 = gtk_frame_new("Mode");
	gtk_widget_show(frame7);
	gtk_box_pack_start (GTK_BOX (frame5box), frame7, TRUE, TRUE, 0);
	frame7box = gtk_vbox_new (TRUE, 7);
	gtk_widget_show (frame7box);
	gtk_container_add(GTK_CONTAINER(frame7), frame7box);
	ie->mode_select = gtk_combo_box_new_text ();
	for (i=0 ;i<4; i++) {
		gtk_combo_box_append_text (GTK_COMBO_BOX (ie->mode_select ),string2[i]);
	}
	gtk_combo_box_set_active (GTK_COMBO_BOX (ie->mode_select), 1);
	gtk_box_pack_start (GTK_BOX (frame7box), ie->mode_select, TRUE, TRUE, 0);
	gtk_widget_show (ie->mode_select);

	/* Lame bitrate */
	frame8 = gtk_frame_new("Bitrate");
	gtk_widget_show(frame8);
	gtk_box_pack_start (GTK_BOX (frame5box), frame8, TRUE, TRUE, 0);
	frame8box = gtk_vbox_new (TRUE, 8);
	gtk_widget_show (frame8box);
	gtk_container_add(GTK_CONTAINER(frame8), frame8box);
	ie->bitrate_select = gtk_combo_box_new_text ();

	for (i=0 ;i<14; i++) {
		gtk_combo_box_append_text (GTK_COMBO_BOX (ie->bitrate_select ),string3[i]);
	}
	gtk_combo_box_set_active (GTK_COMBO_BOX (ie->bitrate_select), 8);
	gtk_box_pack_start (GTK_BOX (frame8box), ie->bitrate_select, TRUE, TRUE, 0);
	gtk_widget_show (ie->bitrate_select);

	/* Lame mp3 id3 tags */
	boxtags = gtk_vbox_new (TRUE, 9);
	gtk_box_pack_start (GTK_BOX (bigbox2), boxtags, TRUE, TRUE, 0);
	gtk_widget_show (boxtags);

	boxtags2 = gtk_vbox_new (TRUE, 9);
	gtk_box_pack_start (GTK_BOX (bigbox2), boxtags2, TRUE, TRUE, 0);
	gtk_widget_show (boxtags2);


	frame9 = gtk_frame_new("MP3 title");
	gtk_widget_show(frame9);
	gtk_box_pack_start (GTK_BOX (boxtags), frame9, TRUE, TRUE, 0);
	frame9box = gtk_vbox_new (TRUE, 9);
	gtk_widget_show (frame9box);
	gtk_container_add(GTK_CONTAINER(frame9), frame9box);
	ie->title = gtk_entry_new_with_max_length (30);
	gtk_box_pack_start (GTK_BOX (frame9box), ie->title, TRUE, TRUE, 0);
	gtk_entry_set_text (GTK_ENTRY(ie->title) ,gpsm_item_label(item));
	gtk_widget_show(ie->title);

	frame10 = gtk_frame_new("MP3 artist");
	gtk_widget_show(frame10);
	gtk_box_pack_start (GTK_BOX (boxtags), frame10, TRUE, TRUE, 0);
	frame10box = gtk_vbox_new (TRUE, 10);
	gtk_widget_show (frame10box);
	gtk_container_add(GTK_CONTAINER(frame10), frame10box);
	ie->artist = gtk_entry_new_with_max_length (30);
	gtk_box_pack_start (GTK_BOX (frame10box), ie->artist, TRUE, TRUE, 0);
	gtk_widget_show(ie->artist);

	frame11 = gtk_frame_new("MP3 album");
	gtk_widget_show(frame11);
	gtk_box_pack_start (GTK_BOX (boxtags), frame11, TRUE, TRUE, 0);
	frame11box = gtk_vbox_new (TRUE, 11);
	gtk_widget_show (frame11box);
	gtk_container_add(GTK_CONTAINER(frame11), frame11box);
	ie->album = gtk_entry_new_with_max_length (30);
	gtk_box_pack_start (GTK_BOX (frame11box), ie->album, TRUE, TRUE, 0);
	gtk_widget_show(ie->album);

	frame12 = gtk_frame_new("MP3 year");
	gtk_widget_show(frame12);
	gtk_box_pack_start (GTK_BOX (boxtags2), frame12, TRUE, TRUE, 0);
	frame12box = gtk_vbox_new (TRUE, 12);
	gtk_widget_show (frame12box);
	gtk_container_add(GTK_CONTAINER(frame12), frame12box);
	ie->year = gtk_entry_new_with_max_length (4);
	gtk_box_pack_start (GTK_BOX (frame12box), ie->year, TRUE, TRUE, 0);
	gtk_widget_show(ie->year);
	
	frame13 = gtk_frame_new("MP3 comment");
	gtk_widget_show(frame13);
	gtk_box_pack_start (GTK_BOX (boxtags2), frame13, TRUE, TRUE, 0);
	frame13box = gtk_vbox_new (TRUE, 13);
	gtk_widget_show (frame13box);
	gtk_container_add(GTK_CONTAINER(frame13), frame13box);
	ie->comment = gtk_entry_new_with_max_length (30);
	gtk_box_pack_start (GTK_BOX (frame13box), ie->comment, TRUE, TRUE, 0);
	gtk_widget_show(ie->comment);
	
	frame14 = gtk_frame_new("MP3 track");
	gtk_widget_show(frame14);
	gtk_box_pack_start (GTK_BOX (boxtags2), frame14, TRUE, TRUE, 0);
	frame14box = gtk_vbox_new (TRUE, 14);
	gtk_widget_show (frame14box);
	gtk_container_add(GTK_CONTAINER(frame14), frame14box);
	ie->track = gtk_entry_new_with_max_length (4);
	gtk_box_pack_start (GTK_BOX (frame14box), ie->track, TRUE, TRUE, 0);
	gtk_widget_show(ie->track);

	frame15 = gtk_frame_new("MP3 genre");
	gtk_widget_show(frame15);
	gtk_box_pack_start (GTK_BOX (boxtags2), frame15, TRUE, TRUE, 0);
	frame15box = gtk_vbox_new (TRUE, 15);
	gtk_widget_show (frame15box);
	gtk_container_add(GTK_CONTAINER(frame15), frame15box);
	ie->genre = gtk_entry_new_with_max_length (30);
	gtk_box_pack_start (GTK_BOX (frame15box), ie->genre, TRUE, TRUE, 0);
	gtk_widget_show(ie->genre);
#endif


#ifdef HAVE_LIBVORBISFILE	
	/*** Ogg Vorbis tab ***/
	label_tab3 = gtk_label_new(_("Ogg Vorbis"));
	gtk_widget_show (label_tab3);
	bigbox3 = gtk_hbox_new (FALSE, 0); 
	gtk_widget_show(bigbox3);
	gtk_notebook_append_page(GTK_NOTEBOOK(ie->notebook), bigbox3 ,label_tab3);
	ie->oggvorbistab_num = (ie->mp3tab_num = 1) ? 2 : 1;
	frame25 = gtk_frame_new("Ogg Vorbis settings (vbr)");
	gtk_widget_show(frame25);
	gtk_box_pack_start (GTK_BOX (bigbox3), frame25, TRUE, TRUE, 0);
	frame25box = gtk_vbox_new (TRUE, 7);
	gtk_widget_show (frame25box);
	gtk_container_add(GTK_CONTAINER(frame25), frame25box);
	
	/* Oggvorbis encoding qual.  */
	frame24 = gtk_frame_new("Quality");
	gtk_widget_show(frame24);
	gtk_box_pack_start (GTK_BOX (frame25box), frame24, TRUE, TRUE, 0);
	frame24box = gtk_vbox_new (TRUE, 7);
	gtk_widget_show (frame24box);
	gtk_container_add(GTK_CONTAINER(frame24), frame24box);
	ie->quality_select_ogg = gtk_combo_box_new_text ();
	for (i=0 ;i<11; i++) {
	  gtk_combo_box_append_text (GTK_COMBO_BOX (ie->quality_select_ogg ),string5[i]);
	}
	gtk_combo_box_set_active (GTK_COMBO_BOX (ie->quality_select_ogg), 4);
	gtk_box_pack_start (GTK_BOX (frame24box), ie->quality_select_ogg, TRUE, TRUE, 0);
	gtk_widget_show (ie->quality_select_ogg);
	
	/* Oggvorbis mode */
	frame17 = gtk_frame_new("Mode");
	gtk_widget_show(frame17);
	gtk_box_pack_start (GTK_BOX (frame25box), frame17, TRUE, TRUE, 0);
	frame17box = gtk_vbox_new (TRUE, 7);
	gtk_widget_show (frame17box);
	gtk_container_add(GTK_CONTAINER(frame17), frame17box);
	ie->mode_select_ogg = gtk_combo_box_new_text ();
	for (i=0 ;i<2; i++) {
	  gtk_combo_box_append_text (GTK_COMBO_BOX (ie->mode_select_ogg ),string4[i]);
	  
	}
	gtk_box_pack_start (GTK_BOX (frame17box), ie->mode_select_ogg, TRUE, TRUE, 0);
	gtk_combo_box_set_active (GTK_COMBO_BOX (ie->mode_select_ogg), 0);
	gtk_widget_show (ie->mode_select_ogg);

	/* empty frame */
	frame26 = gtk_frame_new(" ");
	gtk_widget_show(frame26);
	gtk_box_pack_start (GTK_BOX (frame25box), frame26, TRUE, TRUE, 0);
	frame26box = gtk_vbox_new (TRUE, 7);
	gtk_widget_show (frame26box);
	gtk_container_add(GTK_CONTAINER(frame26), frame26box);

	
	boxtags3 = gtk_vbox_new (TRUE, 9);
	gtk_box_pack_start (GTK_BOX (bigbox3), boxtags3, TRUE, TRUE, 0);
	gtk_widget_show (boxtags3);

	boxtags4 = gtk_vbox_new (TRUE, 9);
	gtk_box_pack_start (GTK_BOX (bigbox3), boxtags4, TRUE, TRUE, 0);
	gtk_widget_show (boxtags4);


	/* Oggvorbis title */
	frame16 = gtk_frame_new("Title");
	gtk_widget_show(frame16);
	gtk_box_pack_start (GTK_BOX( boxtags3), frame16, TRUE, TRUE, 0);
	frame16box = gtk_vbox_new (TRUE, 9);
	gtk_widget_show (frame16box);
	gtk_container_add(GTK_CONTAINER(frame16), frame16box);
	ie->title_ogg = gtk_entry_new_with_max_length (30);
	gtk_box_pack_start (GTK_BOX (frame16box), ie->title_ogg, TRUE, TRUE, 0);
	gtk_entry_set_text (GTK_ENTRY(ie->title_ogg) ,gpsm_item_label(item));
	gtk_widget_show(ie->title_ogg);
	
	/* Oggvorbis artist */
	frame18 = gtk_frame_new("Artist");
	gtk_widget_show(frame18);
	gtk_box_pack_start (GTK_BOX( boxtags3), frame18, TRUE, TRUE, 0);
	frame18box = gtk_vbox_new (TRUE, 9);
	gtk_widget_show (frame18box);
	gtk_container_add(GTK_CONTAINER(frame18), frame18box);
	ie->artist_ogg = gtk_entry_new_with_max_length (30);
	gtk_box_pack_start (GTK_BOX (frame18box), ie->artist_ogg, TRUE, TRUE, 0);
	gtk_widget_show(ie->artist_ogg);

	/* Oggvorbis album */
	frame19 = gtk_frame_new("Album");
	gtk_widget_show(frame19);
	gtk_box_pack_start (GTK_BOX( boxtags3), frame19, TRUE, TRUE, 0);
	frame19box = gtk_vbox_new (TRUE, 9);
	gtk_widget_show (frame19box);
	gtk_container_add(GTK_CONTAINER(frame19), frame19box);
	ie->album_ogg = gtk_entry_new_with_max_length (30);
	gtk_box_pack_start (GTK_BOX (frame19box), ie->album_ogg, TRUE, TRUE, 0);
	gtk_widget_show(ie->album_ogg);

	/* Oggvorbis year */
	frame20 = gtk_frame_new("Year");
	gtk_widget_show(frame20);
	gtk_box_pack_start (GTK_BOX( boxtags4), frame20, TRUE, TRUE, 0);
	frame20box = gtk_vbox_new (TRUE, 9);
	gtk_widget_show (frame20box);
	gtk_container_add(GTK_CONTAINER(frame20), frame20box);
	ie->year_ogg = gtk_entry_new_with_max_length (4);
	gtk_box_pack_start (GTK_BOX (frame20box), ie->year_ogg, TRUE, TRUE, 0);
	gtk_widget_show(ie->year_ogg);

	/* Oggvorbis comment */
	frame21 = gtk_frame_new("Comment");
	gtk_widget_show(frame21);
	gtk_box_pack_start (GTK_BOX(boxtags4), frame21, TRUE, TRUE, 0);
	frame21box = gtk_vbox_new (TRUE, 9);
	gtk_widget_show (frame21box);
	gtk_container_add(GTK_CONTAINER(frame21), frame21box);
	ie->comment_ogg = gtk_entry_new_with_max_length (30);
	gtk_box_pack_start (GTK_BOX (frame21box), ie->comment_ogg, TRUE, TRUE, 0);
	gtk_widget_show(ie->comment_ogg);
	
	/* Oggvorbis track */
	frame22 = gtk_frame_new("Track");
	gtk_widget_show(frame22);
	gtk_box_pack_start (GTK_BOX(boxtags4), frame22, TRUE, TRUE, 0);
	frame22box = gtk_vbox_new (TRUE, 9);
	gtk_widget_show (frame22box);
	gtk_container_add(GTK_CONTAINER(frame22), frame22box);
	ie->track_ogg = gtk_entry_new_with_max_length (3);
	gtk_box_pack_start (GTK_BOX (frame22box), ie->track_ogg, TRUE, TRUE, 0);
	gtk_widget_show(ie->track_ogg);

	/* Oggvorbis genre */
	frame23 = gtk_frame_new("Genre");
	gtk_widget_show(frame23);
	gtk_box_pack_start (GTK_BOX(boxtags4), frame23, TRUE, TRUE, 0);
	frame23box = gtk_vbox_new (TRUE, 9);
	gtk_widget_show (frame23box);
	gtk_container_add(GTK_CONTAINER(frame23), frame23box);
	ie->genre_ogg = gtk_entry_new_with_max_length (30);
	gtk_box_pack_start (GTK_BOX (frame23box), ie->genre_ogg, TRUE, TRUE, 0);
	gtk_widget_show(ie->genre_ogg);


#endif

	gnome_dialog_append_button(GNOME_DIALOG (ie->dialog), _("Export"));
	gnome_dialog_set_default(GNOME_DIALOG(ie->dialog), OK);
	gnome_dialog_append_button(GNOME_DIALOG (ie->dialog), GTK_STOCK_CANCEL);
	gnome_dialog_append_button(GNOME_DIALOG(ie->dialog), GTK_STOCK_HELP);
	//ie->cancelbutton = GTK_WIDGET (g_list_last (GNOME_DIALOG (ie->dialog)->buttons)->data);
	
	gnome_dialog_button_connect(GNOME_DIALOG(ie->dialog), OK,
				    GTK_SIGNAL_FUNC(export_cb), ie);

	gnome_dialog_button_connect(GNOME_DIALOG(ie->dialog), CANCEL, 
				    GTK_SIGNAL_FUNC(exp_cancel_cb), ie);
	gnome_dialog_button_connect(GNOME_DIALOG(ie->dialog), HELP,
				    GTK_SIGNAL_FUNC(glame_help_cb), "The_Export_Dialog");

	g_signal_connect(GTK_OBJECT(ie->otype_combo_box),"changed",GTK_SIGNAL_FUNC(ie_type_menu_cb), ie);

	g_signal_connect(GTK_OBJECT(ie->ocomp_combo_box),  "changed", (GtkSignalFunc)ie_comp_menu_cb, ie); 
	
	// Retrieve tab related to  (eventually) previously chosen file type  
	if (gtk_combo_box_get_active(GTK_COMBO_BOX (ie->otype_combo_box)) == ie->mp3_menu_index ) 
		gtk_notebook_set_current_page(GTK_NOTEBOOK(ie->notebook),ie->mp3tab_num );
	else if (gtk_combo_box_get_active(GTK_COMBO_BOX (ie->otype_combo_box)) == ie->ogg_menu_index )
	       	gtk_notebook_set_current_page(GTK_NOTEBOOK(ie->notebook),ie->oggvorbistab_num );
	else    gtk_notebook_set_current_page(GTK_NOTEBOOK(ie->notebook), 0);
    	
	/* in case we have a non gnome compliant wm */
	g_signal_connect(GTK_OBJECT(ie->dialog),
			   "delete-event",
			   GTK_SIGNAL_FUNC(exp_windowkilled), ie);

	return (GnomeDialog *)dialog;
}

