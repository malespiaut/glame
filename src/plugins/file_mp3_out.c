/*
 * file_io.c
 * $Id: file_mp3_out.c,v 1.3 2004/02/19 21:33:59 ochonpaul Exp $
 *
 * Copyright (C) 1999, 2000, 2004 Alexander Ehlert, Richard Guenther, Daniel Kobras ,Laurent Georget
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

#include <lame/lame.h>

PLUGIN_SET(file_mp3_out, "write_mp3_file")


typedef struct {
	filter_pipe_t *p;
	filter_buffer_t *buf;
	int pos;
} track_t;



static int write_mp3_file_f(filter_t * n)
{
	filter_pipe_t *in;
	filter_port_t *port;
	int eofs, wbpos;
	int i, iat, iass;
	long pos;
	filter_param_t *pos_param;
	int channelCount;
	int sampleRate = 0;
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
	
	
	 /* Limit to 2 input ports*/
	if (channelCount != 2){
	  FILTER_ERROR_RETURN("This filter can only connect to two input port. Insert a render filter if more or less than 2 ports.\n");
	  }
	
	filename =
	    filterparam_val_string(filterparamdb_get_param
				   (filter_paramdb(n), "filename"));
	if (!filename)
		FILTER_ERROR_RETURN("no filename");
	
	
	mp3_file = fopen(filename, "w+");

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
	lame_print_internals(gfp);
	


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
	/* char *xmlparam; */
	

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

	



	filterparamdb_add_param_pos(filter_paramdb(f));

	f->f = write_mp3_file_f;

	plugin_set(pl, PLUGIN_DESCRIPTION, "write a mp3 file");
	plugin_set(pl, PLUGIN_PIXMAP, "output.png");
	plugin_set(pl, PLUGIN_CATEGORY, "Output");
	plugin_set(pl, PLUGIN_GUI_HELP_PATH, "File_I_O");

	filter_register(f, pl);

	return 0;
}
