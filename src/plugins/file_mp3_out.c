/*
 * file_io.c
 * $Id: file_mp3_out.c,v 1.1 2004/02/15 21:56:56 ochonpaul Exp $
 *
 * Copyright (C) 1999, 2000 Alexander Ehlert, Richard Guenther, Daniel Kobras
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

#include <sys/stat.h>
#include <sys/mman.h>
#include <sys/time.h>
#include <sys/types.h>
#include <signal.h>
#include <regex.h>
#include <unistd.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>
#include <math.h>
#include "filter.h"
#include "util.h"
#include "glplugin.h"
#include "glame_types.h"
#include "glame_byteorder.h"
#include "glame_audiofile.h"

#include </usr/local/include/lame/lame.h>

PLUGIN_SET(file_mp3_out, "write_mp3_file")


typedef struct {
	filter_pipe_t *p;
	filter_buffer_t *buf;
	int pos;
} track_t;

typedef struct {
	AFfilehandle file;
	AFframecount frameCount;
	AFfilesetup fsetup;
	int sampleFormat, sampleWidth;
	int channelCount, frameSize;
	int sampleRate;
	int format;
} read_file_private_t;

static int af_typecnt, *af_indices;

static int write_mp3_file_f(filter_t * n)
{
	filter_pipe_t *in;
	filter_port_t *port;
	/* char *filename; */
	int eofs, wbpos;
	int i, iat, iass;
	int filetype;
	long pos;
	filter_param_t *pos_param;

	/* audiofile stuff */
	/*  AFfilehandle    file; */
/*   AFfilesetup     fsetup; */
/*   int             sampleFormat,sampleWidth; */
	int channelCount, compression;
	int sampleRate;
	track_t *track = NULL;
	short int *buffer = NULL;	/* SAMPLE = float */
	int buffer_size, written, frames;


	lame_global_flags *gfp = NULL;
	int ret_code = 0;
	unsigned char mp3_buf[2304 * 2];	/* value from lame source */
	int count = 0;
	char *filename;
	FILE *mp3_file /* = fopen (filename, "w+") */ ;

	channelCount =
	    filterport_nrpipes(filterportdb_get_port
			       (filter_portdb(n), PORTNAME_IN));

	filename =
	    filterparam_val_string(filterparamdb_get_param
				   (filter_paramdb(n), "filename"));
	puts(filename);
	if (!filename)
		FILTER_ERROR_RETURN("no filename");
	mp3_file = fopen(filename, "w+");



	if (channelCount == 0)
		FILTER_ERROR_RETURN("no inputs");

	if (!(track = ALLOCN(channelCount, track_t)))
		FILTER_ERROR_RETURN("no memory");

	iass = 0;
	filterportdb_foreach_port(filter_portdb(n), port) {
		if (filterport_is_output(port))
			continue;
		filterport_foreach_pipe(port, in) {
			for (iat = 0;
			     iat < iass
			     && FILTER_SAMPLEPIPE_MORE_LEFT(track[iat].p,
							    in); iat++);
			for (i = iass; i > iat; i--)
				track[i] = track[i - 1];
			track[iat].p = in;
			if (iass == 0)
				sampleRate = filterpipe_sample_rate(in);
			else if (filterpipe_sample_rate(in) != sampleRate)
				FILTER_ERROR_RETURN
				    ("inconsistent samplerates");
			iass++;
		}
	}



	buffer_size = 2304 * channelCount;	/*from lame source */

	buffer = ALLOCN(buffer_size, short int);
	if (buffer == NULL)
		FILTER_ERROR_CLEANUP("cannot allocate buffer");

	FILTER_AFTER_INIT;
	/* guihack */
	pos_param =
	    filterparamdb_get_param(filter_paramdb(n),
				    FILTERPARAM_LABEL_POS);
	filterparam_val_set_pos(pos_param, 0);
	pos = 0;

	eofs = channelCount;

	for (i = 0; i < channelCount; i++) {
		if (!(track[i].buf = sbuf_get(track[i].p)))
			eofs--;
		track[i].pos = 0;
	}


	/* mp3_buf = ALLOCN(mp3_buf_size, unsigned char); */
	gfp = lame_init();
	ret_code = lame_init_params(gfp);
	if (ret_code < 0)
		DPRINTF("Error on lame init");
	lame_print_config(gfp);


	while (eofs) {
		FILTER_CHECK_STOP;
		wbpos = 0;
		do {
			/* write one interleaved frame to buffer */
			for (i = 0; i < channelCount; i++)
				if (track[i].buf) {
					buffer[wbpos++] =
					    SAMPLE2SHORT(sbuf_buf
							 (track[i].
							  buf)[track[i].
							       pos++]);
					/* Check for end of buffer */
					if (track[i].pos ==
					    sbuf_size(track[i].buf)) {
						sbuf_unref(track[i].buf);
						if (!
						    (track[i].buf =
						     sbuf_get(track[i].p)))
							eofs--;
						track[i].pos = 0;
					}
				} else
					/* if one track stops before another
					 * we have to fill up with zeroes */
					buffer[wbpos++] = 0.0;


		} while ((wbpos < buffer_size) && (eofs));
		frames = wbpos / channelCount;
		
		if (frames > 0) {
			
			
			written =
			    lame_encode_buffer_interleaved(gfp, buffer,
							   frames, mp3_buf,
							   sizeof(mp3_buf)
							   /* mp3_buf_size */
							   );
			
			if (written < 0)
				FILTER_ERROR_STOP
				    ("couldn't write all frames to mp3 buffer.");
			count = fwrite(mp3_buf, 1, written, mp3_file);
			
			if (count != written)
				FILTER_ERROR_STOP
				    ("Error writing mp3 file \n");


			pos += frames;
			filterparam_val_set_pos(pos_param, pos);
		}
	}


	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	written = lame_encode_flush(gfp, mp3_buf, sizeof(mp3_buf));	/* may return one more mp3 frame */
	count = fwrite(mp3_buf, 1, written, mp3_file);
	lame_close(gfp);
	fclose(mp3_file);	/* close the output file */

	/* if (file != AF_NULL_FILEHANDLE) */
/*     afCloseFile(file); */
/*   if (fsetup) */
/*     afFreeFileSetup(fsetup); */
	if (buffer)
		free(buffer);
	if (track)
		free(track);
	FILTER_RETURN;
}

static int write_mp3_file_connect_in(filter_port_t * port,
				     filter_pipe_t * p)
{
	return 0;
}

int write_mp3_file_register(plugin_t * pl)
{
	filter_t *f;
	filter_port_t *in;
	filter_param_t *param;
	char *xmlparam;
	int i;

	if (!(f = filter_creat(NULL)))
		return -1;

	in = filterportdb_add_port(filter_portdb(f), PORTNAME_IN,
				   FILTER_PORTTYPE_SAMPLE,
				   FILTER_PORTFLAG_INPUT,
				   FILTERPORT_DESCRIPTION, "audio stream",
				   FILTERPORT_END);
	in->connect = write_mp3_file_connect_in;
	param =
	    filterparamdb_add_param_string(filter_paramdb(f), "filename",
					   FILTER_PARAMTYPE_FILENAME, NULL,
					   FILTERPARAM_END);

	/* construct xmlstring for filetype parameter */

	af_typecnt =
	    afQueryLong(AF_QUERYTYPE_FILEFMT, AF_QUERY_ID_COUNT, 0, 0, 0);
	af_indices = NULL;

	if (af_typecnt > 0) {
		xmlparam = ALLOCN(4096, char);
		strcat(xmlparam,
		       "<?xml version=\"1.0\" standalone=\"no\"?>"
		       "<!DOCTYPE glade-interface SYSTEM \"http://glade.gnome.org/glade-2.0.dtd\">"
		       "<glade-interface>"
		       "    <widget class=\"GtkOptionMenu\" id=\"widget\">"
		       "      <property name=\"visible\">True</property>"
		       "      <property name=\"can_focus\">True</property>"
		       "      <property name=\"history\">0</property>"
		       "      <child>"
		       "        <widget class=\"GtkMenu\" id=\"menu1\">"
		       "	  <child>"
		       "            <widget class=\"GtkMenuItem\" id=\"item0\">"
		       "              <property name=\"visible\">True</property>"
		       "              <property name=\"label\" translatable=\"yes\">Auto</property>"
		       "              <property name=\"use_underline\">True</property>"
		       "            </widget>" "</child>");

		af_indices =
		    afQueryPointer(AF_QUERYTYPE_FILEFMT, AF_QUERY_IDS, 0,
				   0, 0);
		for (i = 0; i < af_typecnt; i++) {
			char blah[256];
			sprintf(blah,
				"	  <child>"
				"            <widget class=\"GtkMenuItem\" id=\"item%i\">"
				"              <property name=\"visible\">True</property>"
				"              <property name=\"label\" translatable=\"yes\">",
				i + 1);
			strcat(xmlparam, blah);
			strcat(xmlparam,
			       (char *)
			       afQueryPointer(AF_QUERYTYPE_FILEFMT,
					      AF_QUERY_LABEL,
					      af_indices[i], 0, 0));
			strcat(xmlparam,
			       "</property>"
			       "              <property name=\"use_underline\">True</property>"
			       "            </widget>"
			       "          </child>");
		}

		strcat(xmlparam,
		       "        </widget>"
		       "      </child>"
		       "    </widget>" "</glade-interface>");

		filterparamdb_add_param_long(filter_paramdb(f), "filetype",
					     FILTER_PARAMTYPE_LONG, 0,
					     FILTERPARAM_DESCRIPTION,
					     "filetype",
					     FILTERPARAM_GLADEXML,
					     xmlparam, FILTERPARAM_END);
	}

	filterparamdb_add_param_long(filter_paramdb(f), "sampleformat",
				     FILTER_PARAMTYPE_LONG,
				     AF_SAMPFMT_TWOSCOMP,
				     FILTERPARAM_HIDDEN, "FIXME",
				     FILTERPARAM_END);

	filterparamdb_add_param_long(filter_paramdb(f), "samplewidth",
				     FILTER_PARAMTYPE_LONG, 16,
				     FILTERPARAM_HIDDEN, "FIXME",
				     FILTERPARAM_END);

	filterparamdb_add_param_long(filter_paramdb(f), "compression",
				     FILTER_PARAMTYPE_LONG,
				     AF_COMPRESSION_NONE,
				     FILTERPARAM_HIDDEN, "FIXME",
				     FILTERPARAM_END);

	filterparamdb_add_param_pos(filter_paramdb(f));

	f->f = write_mp3_file_f;

	plugin_set(pl, PLUGIN_DESCRIPTION, "write a mp3 file");
	plugin_set(pl, PLUGIN_PIXMAP, "output.png");
	plugin_set(pl, PLUGIN_CATEGORY, "Output");
	plugin_set(pl, PLUGIN_GUI_HELP_PATH, "File_I_O");

	filter_register(f, pl);

	return 0;
}
