/*
 * file_io.c
 * $Id: file_io.c,v 1.77 2002/01/01 18:57:08 richi Exp $
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
 *
 *
 * Generic audiofile reader filter. Every generic reader should honour
 * the per-pipe parameter "position" by just selecting the "best matching"
 * channel of the file for the pipe. Remixing is not required, neither is
 * duplicate channel output. The real position of the stream should be set
 * exact though.
 * Every generic reader should have a
 * - prepare method which does audiofile header reading and checking if
 *   it can handle the file. Fixup of the output pipes type is required, too.
 *   prepare is a unification of the connect_out & fixup_param method.
 * - f method which does the actual reading
 * - cleanup method to cleanup the private shared state
 * Every generic writer should just have a
 * - f method which does all setup and the actual writing, just terminate
 *   with an error if something is wrong
 * A writer should register itself with a regular expression of filenames
 * it wants to handle.
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


PLUGIN_SET(file_io, "read_file write_file")


typedef struct {
	filter_pipe_t   *p;
	filter_buffer_t *buf;
	int             pos;
	int		mapped;
} track_t;

typedef struct {
	int initted;

	AFfilehandle    file;
	AFframecount    frameCount;
	AFfilesetup     fsetup;
	int             sampleFormat,sampleWidth;
	int             channelCount,frameSize;
	int		sampleRate;
	int             format;
} read_file_private_t;
#define RWPRIV(node) ((read_file_private_t *)((node)->priv))
#define RWA(node) RWPRIV(node)


static void read_file_cleanup(glsig_handler_t *h, long sig, va_list va)
{
	filter_t *n;

	GLSIGH_GETARGS1(va, n);
	if (!RWA(n)) {
		DPRINTF("ficken\n");
		return;
	}
	if (RWA(n)->initted) {
		afCloseFile(RWA(n)->file);
		RWA(n)->file = AF_NULL_FILEHANDLE;
	}
	free(RWA(n));
	n->priv = NULL;
}

static int read_file_init(filter_t *n)
{
	read_file_private_t *p;

	if (!(p = ALLOC(read_file_private_t)))
		return -1;
	n->priv = p;
	glsig_add_handler(&n->emitter, GLSIG_FILTER_DELETED,
			  read_file_cleanup, NULL);

	return 0;
}

static int read_file_connect_out(filter_port_t *port, filter_pipe_t *p)
{
	filter_t *n = filterport_filter(port);

	/* no valid file -> some "defaults", only allow 2 connections. */
	if (RWA(n)->file == AF_NULL_FILEHANDLE) {
		if (filterport_nrpipes(port) > 1)
			return -1;
		filterpipe_settype_sample(p, GLAME_DEFAULT_SAMPLERATE,
					  FILTER_PIPEPOS_DEFAULT);
		return 0;
	}

	/* Check to not exceed number of tracks in file. */
	if (filterport_nrpipes(port) >= RWA(n)->channelCount)
		return -1;

	return 0;
}

static void read_file_pos_changed(glsig_handler_t *h, long sig, va_list va)
{
	filter_param_t *param;
	filter_pipe_t *pipe;

	GLSIGH_GETARGS1(va, param);
	pipe = filterparam_get_sourcepipe(param);

	filterpipe_settype_sample(pipe, filterpipe_sample_rate(pipe),
				  filterparam_val_float(param));
	glsig_emit(filterpipe_emitter(pipe), GLSIG_PIPE_CHANGED, pipe);
}

static int read_file_set_filename(filter_param_t *fparam, const void *val)
{
	filter_t *n;
	const char *filename;
	char info[255];
	int ftype, version;

	filename = *((char **)val);
	if (!filename)
		return 0;

	n = filterparam_filter(fparam);
	if (RWA(n)->file != AF_NULL_FILEHANDLE)
		afCloseFile(RWA(n)->file);

	if ((RWA(n)->file=afOpenFile(filename,"r",NULL))==NULL){ 
		DPRINTF("File not found!\n"); 
		return 0; 
	}

	RWA(n)->frameCount=afGetFrameCount(RWA(n)->file, AF_DEFAULT_TRACK);
	sprintf(info, "%d", (int)RWA(n)->frameCount); 
	filterparam_set_property(fparam,"#framecount", info);

	ftype = afGetFileFormat(RWA(n)->file, &version);
	DPRINTF("ftype = %d, version = %d\n", ftype, version);
	sprintf(info, "%d %d", ftype, version);
	filterparam_set_property(fparam,"#format", info);

	RWA(n)->channelCount = afGetChannels(RWA(n)->file, AF_DEFAULT_TRACK);
	sprintf(info, "%d", RWA(n)->channelCount);
	filterparam_set_property(fparam,"#channels", info);

	afGetSampleFormat(RWA(n)->file, AF_DEFAULT_TRACK, &(RWA(n)->sampleFormat), &(RWA(n)->sampleWidth));
	sprintf(info, "%d bit", RWA(n)->sampleWidth);
	filterparam_set_property(fparam,"#quality", info);

	RWA(n)->frameSize = (int) afGetVirtualFrameSize(RWA(n)->file,
	                                         AF_DEFAULT_TRACK, 1);
	sprintf(info, "%d", RWA(n)->frameSize);
	filterparam_set_property(fparam,"#framesize", info);
	
	RWA(n)->sampleRate = (int)afGetRate(RWA(n)->file, AF_DEFAULT_TRACK);
	sprintf(info, "%d Hz", RWA(n)->sampleRate);
	filterparam_set_property(fparam,"#samplerate", info);

	DPRINTF("File %s: %d channel(s) %d bit %s at %d Hz, "
		"framecount %d, framesize %d.\n",
			filename,
			RWA(n)->channelCount, RWA(n)->sampleWidth, 
			RWA(n)->sampleFormat == AF_SAMPFMT_TWOSCOMP ?
			"signed" : 
			RWA(n)->sampleFormat == AF_SAMPFMT_UNSIGNED ? 
			"unsigned" : "unknown",
			RWA(n)->sampleRate, (int)RWA(n)->frameCount, 
			RWA(n)->frameSize);
	return 0;
}

static int read_file_f(filter_t *n)
{
	long frames,i,j;
	filter_pipe_t *p_out;
	filter_port_t *port;
	SAMPLE *s0, *s1;
	long fcnt, cnt;
	long pos;
	filter_param_t *pos_param;
	SAMPLE *buffer;
	track_t *track;

	if (RWA(n)->file == AF_NULL_FILEHANDLE)
		FILTER_ERROR_RETURN("Invalid filename");

	/* seek to start of audiofile */
	afSeekFrame(RWA(n)->file, AF_DEFAULT_TRACK, 0);
	fcnt = RWA(n)->frameCount;
	
	if (afSetVirtualSampleFormat(RWA(n)->file, AF_DEFAULT_TRACK, AF_SAMPFMT_FLOAT, 32)==-1)
		FILTER_ERROR_RETURN("virtual method failed, get newer libaudiofile!");
		
	if (afSetVirtualPCMMapping(RWA(n)->file, AF_DEFAULT_TRACK, 1.0, 0.0, -1.0, 1.0)==-1)
		FILTER_ERROR_RETURN("virtual method failed, get newer libaudiofile!");

	track = ALLOCN(RWA(n)->channelCount, track_t);
	buffer=ALLOCN(GLAME_WBUFSIZE*RWA(n)->channelCount, SAMPLE);

	FILTER_AFTER_INIT;
	pos_param = filterparamdb_get_param(filter_paramdb(n), FILTERPARAM_LABEL_POS);
        filterparam_val_set_pos(pos_param, 0);
	pos = 0;

	while(fcnt){
		FILTER_CHECK_STOP;
		if (!(frames=afReadFrames(RWA(n)->file, AF_DEFAULT_TRACK, 
					  buffer,
					  MIN(GLAME_WBUFSIZE, fcnt))))
			break;
		pos += frames;
		filterparam_val_set_pos(pos_param, pos);
		fcnt-=frames;
		for (i=0; i < RWA(n)->channelCount; i++){
			track[i].buf =
				sbuf_make_private(sbuf_alloc(frames,n));
			track[i].pos = 0;
		}
		i=0;
		while (i < frames*RWA(n)->channelCount)
			for (j=0; j < RWA(n)->channelCount; j++)
				sbuf_buf(track[j].buf)[track[j].pos++] =
					buffer[i++] ;

		for (i=0; i < RWA(n)->channelCount; i++)
			sbuf_queue(track[i].p, track[i].buf);
	}

	filterportdb_foreach_port(filter_portdb(n), port) {
		if (filterport_is_input(port))
			continue;
		filterport_foreach_pipe(port, p_out)
			sbuf_queue(p_out, NULL);
	}

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	if (buffer!=NULL)
		free(buffer);
	if (track!=NULL)
		free(track);

	FILTER_RETURN;
}

int read_file_register(plugin_t *pl)
{
	filter_t *f;
	filter_port_t *p;
	filter_param_t *param;

	if (!(f = filter_creat(NULL)))
		return -1;

	p = filterportdb_add_port(filter_portdb(f), PORTNAME_OUT,
				  FILTER_PORTTYPE_SAMPLE,
				  FILTER_PORTFLAG_OUTPUT,
				  FILTERPORT_DESCRIPTION, "audio stream",
				  FILTERPORT_END);
	p->connect = read_file_connect_out;
	param = filterparamdb_add_param_float(filterport_paramdb(p), "position", 
				  FILTER_PARAMTYPE_POSITION, FILTER_PIPEPOS_DEFAULT,
				  FILTERPARAM_END);
	glsig_add_handler(filterparam_emitter(param), GLSIG_PARAM_CHANGED,
			  read_file_pos_changed, NULL);
	param = filterparamdb_add_param_string(filter_paramdb(f), "filename",
				   FILTER_PARAMTYPE_FILENAME, NULL,
				   FILTERPARAM_END);
	param->set = read_file_set_filename;
	filterparam_set_property(param,FILTER_PARAM_PROPERTY_FILE_FILTER,"*.wav");
	filterparamdb_add_param_pos(filter_paramdb(f));

	f->f = read_file_f;
	f->init = read_file_init;

	plugin_set(pl, PLUGIN_DESCRIPTION, "read a file");
	plugin_set(pl, PLUGIN_PIXMAP, "input.png");
	plugin_set(pl, PLUGIN_CATEGORY, "Input");
	plugin_set(pl, PLUGIN_GUI_HELP_PATH, "File_I_O");
	
	filter_register(f, pl);

	return 0;
}





static int af_typecnt, *af_indices;

static int write_file_f(filter_t *n)
{
	filter_pipe_t *in;
	filter_port_t *port;
	char *filename;
	int eofs,wbpos;
	int i,iat,iass;
	int filetype;
	long pos;
	filter_param_t *pos_param;

	/* audiofile stuff */
	AFfilehandle    file;
	AFfilesetup     fsetup;
	int             sampleFormat,sampleWidth;
	int             channelCount, compression;
	int		sampleRate;
	track_t         *track = NULL;
	SAMPLE          *buffer = NULL;
	int             buffer_size, written, frames;


	channelCount = filterport_nrpipes(filterportdb_get_port(filter_portdb(n), PORTNAME_IN));

	filename = filterparam_val_string(filterparamdb_get_param(filter_paramdb(n), "filename"));

	if (!filename)
		FILTER_ERROR_RETURN("no filename");


	filetype = filterparam_val_int(filterparamdb_get_param(filter_paramdb(n), "filetype"));
	if (filetype==0 )
		filetype = glame_get_filetype_by_name(filename);
	else {
		filetype--;
		if (filetype>=af_typecnt)
			filetype=-1;
		else
			filetype=af_indices[filetype];
	}

	sampleFormat=filterparam_val_int(filterparamdb_get_param(filter_paramdb(n), "sampleformat"));
	sampleWidth=filterparam_val_int(filterparamdb_get_param(filter_paramdb(n), "samplewidth"));
	compression=filterparam_val_int(filterparamdb_get_param(filter_paramdb(n), "compression"));

	if (filetype==-1)
		FILTER_ERROR_RETURN("Filetype not recognized" 
				    " or not supported by libaudiofile");

	if (channelCount==0)
		FILTER_ERROR_RETURN("no inputs");

	if (!(track=ALLOCN(channelCount,track_t)))
		FILTER_ERROR_RETURN("no memory");

	iass=0;
	filterportdb_foreach_port(filter_portdb(n), port) {
		if (filterport_is_output(port))
			continue;
		filterport_foreach_pipe(port, in) {
			for(iat=0;iat<iass && FILTER_SAMPLEPIPE_MORE_LEFT(track[iat].p,in);iat++);
			for(i=iass;i>iat;i--)
				track[i]=track[i-1];
			track[iat].p=in;
			if(iass==0)
				sampleRate=filterpipe_sample_rate(in);
			else 
				if (filterpipe_sample_rate(in)!=sampleRate)
					FILTER_ERROR_RETURN("inconsistent samplerates");
			iass++;
		}
	}
	
	fsetup=afNewFileSetup();
	afInitFileFormat(fsetup, filetype);
	afInitChannels(fsetup, AF_DEFAULT_TRACK, channelCount);
	afInitSampleFormat(fsetup, AF_DEFAULT_TRACK, sampleFormat, sampleWidth);

	afInitRate(fsetup, AF_DEFAULT_TRACK,sampleRate);
	afInitCompression(fsetup, AF_DEFAULT_TRACK, compression);
 
	file=afOpenFile(filename, "w", fsetup);
	
	if (file==AF_NULL_FILEHANDLE)
		FILTER_ERROR_CLEANUP("couldn't open file");

	if (afSetVirtualSampleFormat(file, AF_DEFAULT_TRACK, AF_SAMPFMT_FLOAT, 32)==-1)
		FILTER_ERROR_CLEANUP("virtual method failed, get newer libaudiofile!");
		
	if (afSetVirtualPCMMapping(file, AF_DEFAULT_TRACK, 1.0, 0.0, -1.0, 1.0)==-1)
		FILTER_ERROR_CLEANUP("virtual method failed, get newer libaudiofile!");
	
	buffer_size = 2048*channelCount;

	buffer = ALLOCN(buffer_size, SAMPLE);
	if(buffer==NULL)
		FILTER_ERROR_CLEANUP("cannot allocate buffer");

	FILTER_AFTER_INIT;
	/* guihack */
	pos_param = filterparamdb_get_param(filter_paramdb(n), FILTERPARAM_LABEL_POS);
        filterparam_val_set_pos(pos_param, 0);
	pos = 0;

	eofs=channelCount;
	
	for(i=0;i<channelCount;i++) {
		if (!(track[i].buf=sbuf_get(track[i].p))) eofs--;
		track[i].pos=0;
	}

	while(eofs){
		FILTER_CHECK_STOP;
		wbpos=0;
		do {
			/* write one interleaved frame to buffer */
			for(i=0;i<channelCount;i++)
				if (track[i].buf){
					buffer[wbpos++]=sbuf_buf(track[i].buf)[track[i].pos++];
					/* Check for end of buffer */
					if(track[i].pos==sbuf_size(track[i].buf)){
						sbuf_unref(track[i].buf);
						if (!(track[i].buf=sbuf_get(track[i].p))) eofs--;
						track[i].pos=0;
					}
				} else
					/* if one track stops before another
					 * we have to fill up with zeroes */
					buffer[wbpos++]=0.0;
		} while ((wbpos<buffer_size) && (eofs));
		frames = wbpos/channelCount;
		if (frames>0) {
			written = afWriteFrames(file, AF_DEFAULT_TRACK, buffer, frames);
			if (written!=frames)
				FILTER_ERROR_STOP("couldn't write all frames (disk full?)");
			pos += frames;
			filterparam_val_set_pos(pos_param, pos);
		} 		
	}

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	if (file != AF_NULL_FILEHANDLE)
		afCloseFile(file);
        if (fsetup)
		afFreeFileSetup(fsetup);
	if (buffer)
		free(buffer); 
	if (track)
		free(track);
	FILTER_RETURN;
}

static int write_file_connect_in(filter_port_t *port, filter_pipe_t *p)
{
	return 0;
}

int write_file_register(plugin_t *pl)
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
	in->connect = write_file_connect_in;
	param = filterparamdb_add_param_string(filter_paramdb(f), "filename",
					       FILTER_PARAMTYPE_FILENAME, NULL,
					       FILTERPARAM_END);

	/* construct xmlstring for filetype parameter */

 	af_typecnt = afQueryLong(AF_QUERYTYPE_FILEFMT, AF_QUERY_ID_COUNT,0 ,0 ,0);
	af_indices = NULL;

	if (af_typecnt>0) {
		xmlparam = ALLOCN(256, char);
		strcat(xmlparam, 
		       "<?xml version=\"1.0\"?><GTK-Interface>"
		       "<widget><class>GtkOptionMenu</class>"
		       "<name>widget</name><can_focus>True</can_focus><items>auto\n");
		
		
		af_indices = afQueryPointer(AF_QUERYTYPE_FILEFMT, AF_QUERY_IDS, 0 ,0, 0);	
		for(i=0; i<af_typecnt; i++) {
			strcat(xmlparam, (char*)afQueryPointer(AF_QUERYTYPE_FILEFMT, AF_QUERY_LABEL, af_indices[i] ,0 ,0));
			strcat(xmlparam,"\n");
		}

		strcat(xmlparam, "</items><initial_choice>0</initial_choice></widget></GTK-Interface>");
	
		filterparamdb_add_param_int(filter_paramdb(f),"filetype", 
					    FILTER_PARAMTYPE_INT, 0, 
					    FILTERPARAM_DESCRIPTION,
					    "filetype", 
					    FILTERPARAM_GLADEXML, xmlparam,
					    FILTERPARAM_END);
	}

	filterparamdb_add_param_int(filter_paramdb(f), "sampleformat",
				    FILTER_PARAMTYPE_INT, AF_SAMPFMT_TWOSCOMP,
				    FILTERPARAM_HIDDEN, "FIXME",
				    FILTERPARAM_END);

	filterparamdb_add_param_int(filter_paramdb(f), "samplewidth",
				    FILTER_PARAMTYPE_INT, 16,
				    FILTERPARAM_HIDDEN, "FIXME",
				    FILTERPARAM_END);
	
	filterparamdb_add_param_int(filter_paramdb(f), "compression",
				    FILTER_PARAMTYPE_INT, AF_COMPRESSION_NONE,
				    FILTERPARAM_HIDDEN, "FIXME",
				    FILTERPARAM_END);

	filterparamdb_add_param_pos(filter_paramdb(f));

	f->f = write_file_f;

	plugin_set(pl, PLUGIN_DESCRIPTION, "write a file");
	plugin_set(pl, PLUGIN_PIXMAP, "output.png");
	plugin_set(pl, PLUGIN_CATEGORY, "Output");
	plugin_set(pl, PLUGIN_GUI_HELP_PATH, "File_I_O");
	
	filter_register(f, pl);

	return 0;
}
