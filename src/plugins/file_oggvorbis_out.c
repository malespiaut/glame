/*
 * file_oggvorbis_out.c
 *
 * Copyright (C) 1999, 2000, 2004 Alexander Ehlert, Richard Guenther, Daniel Kobras, Laurent Georget
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


#ifdef HAVE_LIBVORBISFILE
#include <vorbis/codec.h>
#include <vorbis/vorbisfile.h>
#include <vorbis/vorbisenc.h>
#endif

PLUGIN_SET(file_oggvorbis_out, "write_oggvorbis_file")

typedef struct {
	filter_pipe_t *p;
	filter_buffer_t *buf;
	int pos;
} track_t;


static int write_oggvorbis_file_f(filter_t * n)
{
	filter_pipe_t *in;
	filter_port_t *port;
	int eofs, wbpos;
	int i, j, iat, iass;
	long pos;
	filter_param_t *pos_param;
	int channelCount;
	int sampleRate = 0;

	char *filename;
	FILE *oggvorbis_file;
	track_t *track = NULL;
	int vorbis_analysis_buffer_size, written, frames;
	int ret, eos = 0;
	float quality;
	ogg_stream_state os;	/* take physical pages, weld into a logical
				   stream of packets */
	ogg_page og;		/* one Ogg bitstream page.  Vorbis packets are inside */
	ogg_packet op;          /* one raw packet of data for decode */
	vorbis_info vi;		/* struct that stores all the static vorbis bitstream
				   settings */
	vorbis_comment vc;	/* struct that stores all the user comments */
	vorbis_dsp_state vd;	/* central working state for the packet->PCM decoder */
	vorbis_block vb;	/* local working space for packet->PCM decode */


	channelCount =
	  filterport_nrpipes(filterportdb_get_port
			     (filter_portdb(n), PORTNAME_IN));

	/* Limit to 1 or 2 input ports . Ogg can encode more channels, but player i know can't read . */
	if (channelCount > 2){
	  FILTER_ERROR_RETURN(" This filter can only connect to one or two input port. Insert a render filter if more or less than 2 ports.");
	}

	filename =
	  filterparam_val_string(filterparamdb_get_param
				 (filter_paramdb(n), "filename"));
	if (!filename)
	  FILTER_ERROR_RETURN("no filename");

	oggvorbis_file = fopen(filename, "w+");
	if (!oggvorbis_file)
	  FILTER_ERROR_RETURN("can't open/create file ");

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

	/* Vorbis */
	vorbis_info_init(&vi);
	quality = filterparam_val_long(filterparamdb_get_param(filter_paramdb(n), "vorbis encoding quality"));
	ret = vorbis_encode_init_vbr(&vi, eofs, 44100, quality/10);
	if (ret)
	  FILTER_ERROR_CLEANUP
	    ("couldn't init vorbis (bad parameters ?).");

	/* add  comments  */
	vorbis_comment_init(&vc);
	vorbis_comment_add_tag(&vc, "ENCODER", "Glame");
	vorbis_comment_add_tag(&vc, "TITLE",filterparam_val_string(filterparamdb_get_param(filter_paramdb(n), "Title")));
	vorbis_comment_add_tag(&vc, "ARTIST",filterparam_val_string(filterparamdb_get_param(filter_paramdb(n), "Artist")));
	vorbis_comment_add_tag(&vc, "ALBUM",filterparam_val_string(filterparamdb_get_param(filter_paramdb(n), "Album")));
	vorbis_comment_add_tag(&vc, "YEAR",filterparam_val_string(filterparamdb_get_param(filter_paramdb(n), "Year")));
	vorbis_comment_add_tag(&vc, "COMMENT",filterparam_val_string(filterparamdb_get_param(filter_paramdb(n), "Comment")));
	vorbis_comment_add_tag(&vc, "TRACK",filterparam_val_string(filterparamdb_get_param(filter_paramdb(n), "Track")));
	vorbis_comment_add_tag(&vc, "GENRE",filterparam_val_string(filterparamdb_get_param(filter_paramdb(n), "Genre")));
	/* puts(filterparam_val_string(filterparamdb_get_param(filter_paramdb(n), "Title"))); */	
	/* set up the analysis state and auxiliary encoding storage */
	vorbis_analysis_init(&vd, &vi);
	vorbis_block_init(&vd, &vb);

	/* set up our packet->stream encoder */
	/* pick a random serial number; that way we can more likely build
	   chained streams just by concatenation */
	srand(time(NULL));
	ogg_stream_init(&os, rand());

	/* Vorbis streams begin with three headers; the initial header (with
	   most of the codec setup parameters) which is mandated by the Ogg
	   bitstream spec.  The second header holds any comment fields.  The
	   third header holds the bitstream codebook */
	{
	  ogg_packet header;
	  ogg_packet header_comm;
	  ogg_packet header_code;

	  vorbis_analysis_headerout(&vd, &vc, &header, &header_comm,
				    &header_code);
	  ogg_stream_packetin(&os, &header);	/* automatically placed in its own page */
	  ogg_stream_packetin(&os, &header_comm);
	  ogg_stream_packetin(&os, &header_code);

	  /* This ensures the actual
	   * audio data will start on a new page, as per spec
	   */
	  while (1) {
	    int result = ogg_stream_flush(&os, &og);
	    if (result == 0)
	      break;
	    fwrite(og.header, 1, og.header_len, oggvorbis_file);
	    fwrite(og.body, 1, og.body_len, oggvorbis_file);
	  }
	}

	vorbis_analysis_buffer_size = 2048* channelCount;
	eos = 0;
	while (!eos) {
	  FILTER_CHECK_STOP;
	  wbpos = 0;
	  /* expose the buffer to submit data */
	    float **analysis_buffer =
	      vorbis_analysis_buffer(&vd, vorbis_analysis_buffer_size );

	  
	    /* uninterleave samples to buffer */
	    do {
	      for (j = 0; j < channelCount; j++){
		if (track[j].buf) {
		  analysis_buffer[j][wbpos] = sbuf_buf(track[j].buf)[track[j].pos++]; /* FIXME ? */

		  /* printf("pre eofs %i   eos %i  wbpos %i j %i\n",eofs,eos,wbpos,j); */
/* 		  printf( "track[j].pos %i analysis_buffer[j][wbpos] %f  sbuf_buf(track[j].buf)[track[j].pos] %f\n\n", */
/*                   track[j].pos, analysis_buffer[j][wbpos], sbuf_buf(track[j].buf)[track[j].pos]); */
		  
		  /* Check for end of buffer */
		  if (track[j].pos == (sbuf_size(track[j].buf))-1) /* FIXME ? */
		    { 
		      sbuf_unref(track[j].buf);
		      if (!(track[j].buf = sbuf_get(track[j].p)))
			eofs--;
		      track[j].pos =0;
		    }
		} else
		  /* if one track stops before another
		   * we have to fill up with zeroes */
		  analysis_buffer[j][wbpos] = 0.0;

	      } 
	      wbpos++; 
	    }while ((wbpos < (vorbis_analysis_buffer_size/channelCount)) && (eofs));

	    vorbis_analysis_wrote(&vd,wbpos);
	    /* printf ("frames %i \n",frames); */
	    frames = wbpos;
	    if (frames == 1) vorbis_analysis_wrote(&vd,0); /*end of samples, prepare marking end of of stream*/
	    /* get one block then analyse*/
	    while(vorbis_analysis_blockout(&vd,&vb)==1){
	      
	      /* analysis, assume we want to use bitrate management, and create a packet */
	      vorbis_analysis(&vb,NULL);
	      vorbis_bitrate_addblock(&vb);
			  
	      while(vorbis_bitrate_flushpacket(&vd,&op)){
	
		/* weld the packet into the bitstream */
		/* if (!eofs)	op.e_o_s = 1; */
		ogg_stream_packetin(&os,&op);
	
		/* write out pages (if any) in file*/
		while(!eos){
		  int result=ogg_stream_pageout(&os,&og);
		  if(result==0)break;
		  written = fwrite(og.header,1,og.header_len, oggvorbis_file);
		  written +=fwrite(og.body,1,og.body_len, oggvorbis_file);
		  if(written != og.header_len + og.body_len) 
		    { FILTER_ERROR_CLEANUP("cannot write to file"); }
		  
		  if(ogg_page_eos(&og))  eos=1;
		  
		}
	      }
	    }
	    pos += frames;
	    filterparam_val_set_pos(pos_param, pos);
				
	   
	}
	

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	ogg_stream_clear(&os);
	vorbis_block_clear(&vb);
	vorbis_dsp_clear(&vd);
	vorbis_comment_clear(&vc);
	vorbis_info_clear(&vi);
	fclose(oggvorbis_file);
	/* if (analysis_buffer) */
/* 	  free(analysis_buffer); */
	if (track)
	  free(track);
	FILTER_RETURN;
		
}


static int write_oggvorbis_file_connect_in(filter_port_t * port,
					   filter_pipe_t * p)
{

	return 0;
}


int write_oggvorbis_file_register(plugin_t * pl)
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
	in->connect = write_oggvorbis_file_connect_in;

	param =
	    filterparamdb_add_param_string(filter_paramdb(f), "filename",
					   FILTER_PARAMTYPE_FILENAME, NULL,
					   FILTERPARAM_END);
	param =
	    filterparamdb_add_param_string(filter_paramdb(f),
					   "Title",
					   FILTER_PARAMTYPE_STRING, "",
					   FILTERPARAM_END);
	param =
	    filterparamdb_add_param_string(filter_paramdb(f),
					   "Artist",
					   FILTER_PARAMTYPE_STRING, "",
					   FILTERPARAM_END);
	param =
	    filterparamdb_add_param_string(filter_paramdb(f),
					   "Album",
					   FILTER_PARAMTYPE_STRING, "",
					   FILTERPARAM_END);
	param =
	    filterparamdb_add_param_string(filter_paramdb(f),
					   "Year",
					   FILTER_PARAMTYPE_STRING, "",
					   FILTERPARAM_END);
	param =
	    filterparamdb_add_param_string(filter_paramdb(f),
					   "Comment",
					   FILTER_PARAMTYPE_STRING, "",
					   FILTERPARAM_END);
	param =
	    filterparamdb_add_param_string(filter_paramdb(f),
					   "Track",
					   FILTER_PARAMTYPE_STRING, "",
					   FILTERPARAM_END);
	param =
	    filterparamdb_add_param_string(filter_paramdb(f),
					   "Genre",
					   FILTER_PARAMTYPE_STRING, "",
					   FILTERPARAM_END);

	param =
	    filterparamdb_add_param_long(filter_paramdb(f),
					 "vorbis encoding quality",
					 FILTER_PARAMTYPE_LONG, 4,
					 FILTERPARAM_DESCRIPTION,
					 "Vorbis encoding quality (vbr)\n",
					 FILTERPARAM_GLADEXML,
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
					 "              <property name=\"label\" translatable=\"yes\">0 (roughly 64kb/s)</property>"
					 "              <property name=\"use_underline\">True</property>"
					 "            </widget>"
					 "          </child>"
					 "	  <child>"
					 "            <widget class=\"GtkMenuItem\" id=\"item1\">"
					 "              <property name=\"visible\">True</property>"
					 "              <property name=\"label\" translatable=\"yes\">0.1 (roughly 80kb/s)</property>"
					 "              <property name=\"use_underline\">True</property>"
					 "            </widget>"
					 "          </child>"
					 "	  <child>"
					 "            <widget class=\"GtkMenuItem\" id=\"item2\">"
					 "              <property name=\"visible\">True</property>"
					 "              <property name=\"label\" translatable=\"yes\">0.2 (roughly 96kb/s)</property>"
					 "              <property name=\"use_underline\">True</property>"
					 "            </widget>"
					 "          </child>"
					 "	  <child>"
					 "            <widget class=\"GtkMenuItem\" id=\"item3\">"
					 "              <property name=\"visible\">True</property>"
					 "              <property name=\"label\" translatable=\"yes\">0.3 (roughly 112kb/s)</property>"
					 "              <property name=\"use_underline\">True</property>"
					 "            </widget>"
					 "          </child>"
					 "           <child>"
					 "            <widget class=\"GtkMenuItem\" id=\"item4\">"
					 "              <property name=\"visible\">True</property>"
					 "              <property name=\"label\" translatable=\"yes\">0.4 (roughly 128kb/s)</property>"
					 "              <property name=\"use_underline\">True</property>"
					 "            </widget>"
					 "          </child>"
					 "          <child>"
					 "            <widget class=\"GtkMenuItem\" id=\"item5\">"
					 "              <property name=\"visible\">True</property>"
					 "              <property name=\"label\" translatable=\"yes\">0.5 (roughly 160kb/s)</property>"
					 "              <property name=\"use_underline\">True</property>"
					 "            </widget>"
					 "          </child>"
					 "          <child>"
					 "            <widget class=\"GtkMenuItem\" id=\"item6\">"
					 "              <property name=\"visible\">True</property>"
					 "              <property name=\"label\" translatable=\"yes\">0.6 (roughly 192kb/s)</property>"
					 "              <property name=\"use_underline\">True</property>"
					 "            </widget>"
					 "          </child>"
					 "          <child>"
					 "            <widget class=\"GtkMenuItem\" id=\"item7\">"
					 "              <property name=\"visible\">True</property>"
					 "              <property name=\"label\" translatable=\"yes\">0.7 (roughly 224kb/s)</property>"
					 "              <property name=\"use_underline\">True</property>"
					 "            </widget>"
					 "          </child>"
					 "          <child>"
					 "            <widget class=\"GtkMenuItem\" id=\"item8\">"
					 "              <property name=\"visible\">True</property>"
					 "              <property name=\"label\" translatable=\"yes\">0.8 (roughly 256kb/s)</property>"
					 "              <property name=\"use_underline\">True</property>"
					 "            </widget>"
					 "          </child>"
					 "          <child>"
					 "            <widget class=\"GtkMenuItem\" id=\"item9\">"
					 "              <property name=\"visible\">True</property>"
					 "              <property name=\"label\" translatable=\"yes\">0.9 (roughly 320kb/s)</property>"
					 "              <property name=\"use_underline\">True</property>"
					 "            </widget>"
					 "          </child>"
					 "          <child>"
					 "            <widget class=\"GtkMenuItem\" id=\"item10\">"
					 "              <property name=\"visible\">True</property>"
					 "              <property name=\"label\" translatable=\"yes\">1 (roughly 500kb/s)</property>"
					 "              <property name=\"use_underline\">True</property>"
					 "            </widget>"
					 "          </child>"
					 "        </widget>"
					 "      </child>"
					 "    </widget>"
					 "</glade-interface>",
					 FILTERPARAM_LABEL,
					 "Vorbis encoding quality (vbr)",
					 FILTERPARAM_END);

	

	filterparamdb_add_param_pos(filter_paramdb(f));

	f->f = write_oggvorbis_file_f;

	plugin_set(pl, PLUGIN_DESCRIPTION, "write a OggVorbis file");
	plugin_set(pl, PLUGIN_PIXMAP, "output.png");
	plugin_set(pl, PLUGIN_CATEGORY, "Output");
	plugin_set(pl, PLUGIN_GUI_HELP_PATH, "File_I_O");

	filter_register(f, pl);

	return 0;
}
