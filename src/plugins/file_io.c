/*
 * file_io.c
 * $Id: file_io.c,v 1.73 2001/12/03 23:20:44 mag Exp $
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


int af_read_prepare(filter_t *n, const char *filename);
int af_read_connect(filter_t *n, filter_pipe_t *p);
int af_read_f(filter_t *n);
void af_read_cleanup(filter_t *n);
int af_write_f(filter_t *n);

PLUGIN_SET(file_io, "read_file write_file")


typedef struct {
	struct glame_list_head list;
	int (*prepare)(filter_t *, const char *);
	int (*connect)(filter_t *, filter_pipe_t *);
	int (*f)(filter_t *);
	void (*cleanup)(filter_t *);
        const char *regexp;
} rw_t;

typedef struct {
	filter_pipe_t   *p;
	filter_buffer_t *buf;
	int             pos;
	int		mapped;
} track_t;

typedef struct {
	rw_t *rw;
	int initted;
	union {
	        /* put your shared state stuff here */
		struct {
			AFfilehandle    file;
			AFframecount    frameCount;
		        AFfilesetup     fsetup;
			int             sampleFormat,sampleWidth;
			int             channelCount,frameSize;
			int		sampleRate;
		        int             format;
			track_t         *track;
		} audiofile;
	} u;
} rw_private_t;
#define RWPRIV(node) ((rw_private_t *)((node)->priv))
#define RWA(node) (RWPRIV(node)->u.audiofile)
#define RWM(node) (RWPRIV(node)->u.lame)

/* the readers & the writers list */
static struct glame_list_head readers;

static rw_t *add_rw(int (*prepare)(filter_t *, const char *),
		    int (*connect)(filter_t *, filter_pipe_t *),
		    int (*f)(filter_t *),
		    void (*cleanup)(filter_t *),
		    const char *regexp)
{
	rw_t *rw;

	if (!prepare && !f)
		return NULL;
	if (!(rw = ALLOC(rw_t)))
		return NULL;
	GLAME_INIT_LIST_HEAD(&rw->list);
	rw->prepare = prepare;
	rw->connect = connect;
	rw->f = f;
	rw->cleanup = cleanup;
	if (regexp)
		rw->regexp = strdup(regexp);

	return rw;
}
static int add_reader(int (*prepare)(filter_t *, const char *),
		      int (*connect)(filter_t *, filter_pipe_t *),
		      int (*f)(filter_t *),
		      void (*cleanup)(filter_t *))
{
	rw_t *rw;

	if (!(rw = add_rw(prepare, connect, f, cleanup, NULL)))
	        return -1;
	glame_list_add(&rw->list, &readers);
	return 0;
}

/* generic read&write methods */
static void rw_file_cleanup(glsig_handler_t *h, long sig, va_list va)
{
	filter_t *n;

	GLSIGH_GETARGS1(va, n);
	if (!RWPRIV(n)) {
		DPRINTF("ficken\n");
		return;
	}
	if (RWPRIV(n)->rw
	    && RWPRIV(n)->rw->cleanup 
	    && RWPRIV(n)->initted)
		RWPRIV(n)->rw->cleanup(n);
	free(RWPRIV(n));
	n->priv = NULL;
}
static int rw_file_init(filter_t *n)
{
	rw_private_t *p;

	if (!(p = ALLOC(rw_private_t)))
		return -1;
	n->priv = p;
	glsig_add_handler(&n->emitter, GLSIG_FILTER_DELETED,
			  rw_file_cleanup, NULL);

	return 0;
}

/* read methods */
static int read_file_f(filter_t *n)
{
	/* require set filename (a selected reader) and
	 * at least one connected output. */
	if (!RWPRIV(n)->initted)
		FILTER_ERROR_RETURN("invalid file");
	if (!filterport_get_pipe(filterportdb_get_port(filter_portdb(n), PORTNAME_OUT)))
		FILTER_ERROR_RETURN("no outputs");
	return RWPRIV(n)->rw->f(n);
}
static int read_file_connect_out(filter_port_t *port, filter_pipe_t *p)
{
	filter_t *n = filterport_filter(port);

	/* no reader -> no filename -> some "defaults".
	 * only allow 2 connections. */
	if (!RWPRIV(n)->rw) {
		if (filterport_nrpipes(port) > 1)
			return -1;
		filterpipe_settype_sample(p, GLAME_DEFAULT_SAMPLERATE,
					  FILTER_PIPEPOS_DEFAULT);
		return 0;
	}

	/* pass request to readers prepare, it can reject the
	 * connection, but not the file here. */
	return RWPRIV(n)->rw->connect(n, p);
}

static void read_file_fixup_pipe(glsig_handler_t *h, long sig, va_list va) {
	filter_t	*n;
	filter_pipe_t	*pipe;
	
	GLSIGH_GETARGS1(va, pipe);
	n = filterport_filter(filterpipe_source(pipe));
	if (RWPRIV(n) && RWPRIV(n)->rw) 
		RWPRIV(n)->rw->connect(n, pipe);
}

static int read_file_setup_param(filter_param_t *param, const void *val) 
{
	filter_t *n = filterparam_filter(param);
	filter_pipe_t *p;
	filter_port_t *port;
	rw_t *r;
	char *filename;

	if (*((char**)val)==NULL)
		return -1;

	/* only position param change? - we only care for initted rw. */
	if (RWPRIV(n)->rw
	    && strcmp("position", filterparam_label(param)) == 0) {
		p = filterparam_get_sourcepipe(param);
		if (RWPRIV(n)->rw->connect(n, p) == -1)
			PANIC("Uh? Reject pipe that previously was ok?");
		glsig_emit(&p->emitter, GLSIG_PIPE_CHANGED, p);
		return 0;
	
        /* filename change! */
	} else {
		filename = *((char**)val);
		DPRINTF("filename change to %s\n", filename);
		/* check actual reader */
		if (RWPRIV(n)->rw) {
			/* cleanup previous stuff */
			if (RWPRIV(n)->initted
			    && RWPRIV(n)->rw->cleanup)
				RWPRIV(n)->rw->cleanup(n);
			RWPRIV(n)->initted = 0;
			/* deleted reuse here, because libmp3lame
			 * detects wav as mp3 on change */
		}

		RWPRIV(n)->rw = NULL;
		RWPRIV(n)->initted = 0;

		/* search for applicable reader */
		glame_list_foreach(&readers, rw_t, list, r) {
			if (r->prepare(n, filename) != -1) {
				RWPRIV(n)->rw = r;
				RWPRIV(n)->initted = 1;
				goto reconnect;
			}
		}

		/* no reader found */
		return -1;
	}

 reconnect:
	/* re-connect all pipes */
	filterportdb_foreach_port(filter_portdb(n), port) {
		if (filterport_is_input(port))
			continue;
		filterport_foreach_pipe(port, p) {
			if (RWPRIV(n)->rw->connect(n, p) == -1) {
				filterpipe_delete(p);
				goto reconnect;
			}
			glsig_emit(&p->emitter, GLSIG_PIPE_CHANGED, p);
		}
	}
	return 0;
}


static int write_file_connect_in(filter_port_t *port, filter_pipe_t *p)
{
	/* So why is there no write_file_connect_in?? Do we really
	 * support any number of inputs? Seems all is _f() time... */
	return 0;
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
	param->set = read_file_setup_param;
	param = filterparamdb_add_param_string(filter_paramdb(f), "filename",
				   FILTER_PARAMTYPE_FILENAME, NULL,
				   FILTERPARAM_END);
	param->set = read_file_setup_param;
	filterparam_set_property(param,FILTER_PARAM_PROPERTY_FILE_FILTER,"*.wav");
	filterparamdb_add_param_pos(filter_paramdb(f));

	f->f = read_file_f;
	f->init = rw_file_init;

	glsig_add_handler(&f->emitter, GLSIG_PIPE_DELETED,
			  read_file_fixup_pipe, NULL);

	plugin_set(pl, PLUGIN_DESCRIPTION, "read a file");
	plugin_set(pl, PLUGIN_PIXMAP, "input.png");
	plugin_set(pl, PLUGIN_CATEGORY, "Input");
	plugin_set(pl, PLUGIN_GUI_HELP_PATH, "File_I_O");
	
	filter_register(f, pl);

	return 0;
}



int file_io_register(plugin_t *p)
{
	GLAME_INIT_LIST_HEAD(&readers);

	add_reader(af_read_prepare, af_read_connect,
		   af_read_f, af_read_cleanup);

	return 0;
}



/* The actual readers and writers.
 */

int af_read_prepare(filter_t *n, const char *filename)
{
	filter_param_t *fparam;
	char info[255];
	int ftype, version;

	fparam = filterparamdb_get_param(filter_paramdb(n), "filename");

	DPRINTF("Using audiofile library\n");
	if ((RWA(n).file=afOpenFile(filename,"r",NULL))==NULL){ 
		DPRINTF("File not found!\n"); 
		return -1; 
	}


	RWA(n).frameCount=afGetFrameCount(RWA(n).file, AF_DEFAULT_TRACK);
	sprintf(info, "%d", (int)RWA(n).frameCount); 
	filterparam_set_property(fparam,"#framecount", info);

	ftype = afGetFileFormat(RWA(n).file, &version);
	DPRINTF("ftype = %d, version = %d\n", ftype, version);
	sprintf(info, "%d %d", ftype, version);
	filterparam_set_property(fparam,"#format", info);

	RWA(n).channelCount = afGetChannels(RWA(n).file, AF_DEFAULT_TRACK);
	sprintf(info, "%d", RWA(n).channelCount);
	filterparam_set_property(fparam,"#channels", info);

	afGetSampleFormat(RWA(n).file, AF_DEFAULT_TRACK, &(RWA(n).sampleFormat), &(RWA(n).sampleWidth));
	sprintf(info, "%d bit", RWA(n).sampleWidth);
	filterparam_set_property(fparam,"#quality", info);

	RWA(n).frameSize = (int) afGetVirtualFrameSize(RWA(n).file,
	                                         AF_DEFAULT_TRACK, 1);
	sprintf(info, "%d", RWA(n).frameSize);
	filterparam_set_property(fparam,"#framesize", info);
	
	RWA(n).sampleRate = (int)afGetRate(RWA(n).file, AF_DEFAULT_TRACK);
	sprintf(info, "%d Hz", RWA(n).sampleRate);
	filterparam_set_property(fparam,"#samplerate", info);
		
	if (!(RWA(n).track=ALLOCN(RWA(n).channelCount,track_t))){
		DPRINTF("Couldn't allocate track buffer\n");
		return -1;
	}

	DPRINTF("File %s: %d channel(s) %d bit %s at %d Hz, "
		"framecount %d, framesize %d.\n",
			filename,
			RWA(n).channelCount, RWA(n).sampleWidth, 
			RWA(n).sampleFormat == AF_SAMPFMT_TWOSCOMP ?
			"signed" : 
			RWA(n).sampleFormat == AF_SAMPFMT_UNSIGNED ? 
			"unsigned" : "unknown",
			RWA(n).sampleRate, (int)RWA(n).frameCount, 
			RWA(n).frameSize);
	return 0;
}

int af_read_connect(filter_t *n, filter_pipe_t *p)
{
	int i, deleted=1;
	filter_port_t	*outp;
	filter_pipe_t	*out;
	
	outp = filterportdb_get_port(filter_portdb(n), PORTNAME_OUT);
	filterport_foreach_pipe(outp, out) {
		if (p == out)
			deleted = 0;
	}
	
	if (deleted == 1) {
		for(i=0; i<RWA(n).channelCount; i++)
			if (RWA(n).track[i].p == p) {
				DPRINTF("unmapped channel %d\n", i);
				RWA(n).track[i].mapped = 0;
				RWA(n).track[i].p = NULL;
				return 0;
			}
	}
	
	for(i=0;(i<RWA(n).channelCount) && (RWA(n).track[i].mapped);i++);
	if (i>=RWA(n).channelCount){
		/* Check if track is already mapped ?!
		 * Would be strange, but ... 
		 * - nope, not strange, connect gets called for each
		 *   already connected pipes at parameter change, too!
		 *   (as for just pipe parameter change)
		 * - you should fixup, i.e. re-route perhaps, reject, whatever
		 *   in this case
                 */
		for(i=0;i<RWA(n).channelCount;i++)
			if ((RWA(n).track[i].mapped) && RWA(n).track[i].p==p){
				return 0; 
			}
		return -1;
	} else {
		/* Moah! what is this? does libaudiofile not provide
		 * some "direct" information on position??
		 */
		if (RWA(n).channelCount!=1)
			filterpipe_settype_sample(p,RWA(n).sampleRate,
				(M_PI/(RWA(n).channelCount-1))*i+FILTER_PIPEPOS_LEFT);
		else
			filterpipe_settype_sample(p,RWA(n).sampleRate,FILTER_PIPEPOS_CENTRE);
		RWA(n).track[i].p=p;
		RWA(n).track[i].mapped=1;
	}	
	return 0;	
}

int af_read_f(filter_t *n)
{
	int frames,i,j;
	filter_pipe_t *p_out;
	filter_port_t *port;
	SAMPLE *s0, *s1;
	int fcnt, cnt;
	long pos;
	filter_param_t *pos_param;
	SAMPLE		*buffer;

	/* seek to start of audiofile */
	afSeekFrame(RWA(n).file, AF_DEFAULT_TRACK, 0);
	fcnt = RWA(n).frameCount;
	
	if (afSetVirtualSampleFormat(RWA(n).file, AF_DEFAULT_TRACK, AF_SAMPFMT_FLOAT, 32)==-1)
		FILTER_ERROR_RETURN("virtual method failed, get newer libaudiofile!");
		
	if (afSetVirtualPCMMapping(RWA(n).file, AF_DEFAULT_TRACK, 1.0, 0.0, -1.0, 1.0)==-1)
		FILTER_ERROR_RETURN("virtual method failed, get newer libaudiofile!");

	buffer=ALLOCN(GLAME_WBUFSIZE*RWA(n).channelCount, SAMPLE);

	FILTER_AFTER_INIT;
	pos_param = filterparamdb_get_param(filter_paramdb(n), FILTERPARAM_LABEL_POS);
        filterparam_val_set_pos(pos_param, 0);
	pos = 0;

	while(fcnt){
		FILTER_CHECK_STOP;
		if (!(frames=afReadFrames(RWA(n).file, AF_DEFAULT_TRACK, 
					  buffer,
					  MIN(GLAME_WBUFSIZE, fcnt))))
			break;
		pos += frames;
		filterparam_val_set_pos(pos_param, pos);
		fcnt-=frames;
		for (i=0; i < RWA(n).channelCount; i++){
			RWA(n).track[i].buf =
				sbuf_make_private(sbuf_alloc(frames,n));
			RWA(n).track[i].pos = 0;
		}
		i=0;
		while (i < frames*RWA(n).channelCount)
			for (j=0; j < RWA(n).channelCount; j++)
				sbuf_buf(RWA(n).track[j].buf)[RWA(n).track[j].pos++] =
					buffer[i++] ;

		for (i=0; i < RWA(n).channelCount; i++)
			sbuf_queue(RWA(n).track[i].p, RWA(n).track[i].buf);
	}

	filterportdb_foreach_port(filter_portdb(n), port) {
		if (filterport_is_input(port))
			continue;
		filterport_foreach_pipe(port, p_out)
			sbuf_queue(p_out, NULL);
	}

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	if(buffer!=NULL)
		free(buffer);

	return 0;
}

void af_read_cleanup(filter_t *n)
{
	free(RWA(n).track);
	afCloseFile(RWA(n).file);
	memset(&(RWPRIV(n)->u), 0, sizeof(RWPRIV(n)->u));
}


static int af_typecnt, *af_indices;

int write_file_f(filter_t *n)
{
	filter_pipe_t *in;
	filter_port_t *port;
	char *filename;
	int res=-1, failed=0;
	int eofs,wbpos;
	int i,iat,iass;
	int filetype;
	char *errstring = "write file failed";

	/* audiofile stuff */
	AFfilehandle    file;
	AFfilesetup     fsetup;
	int             sampleFormat,sampleWidth;
	int             channelCount, compression;
	int		sampleRate;
	track_t         *track;
	SAMPLE          *buffer;
	int             buffer_size, written, frames;

	channelCount = filterport_nrpipes(filterportdb_get_port(filter_portdb(n), PORTNAME_IN));

	filename = filterparam_val_string(filterparamdb_get_param(filter_paramdb(n), "filename"));

	if (!filename)
		FILTER_ERROR_RETURN("no filename");


	filetype = filterparam_val_int(filterparamdb_get_param(filter_paramdb(n), "filetype"));
	if (filetype==0)
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
	
	if (file==AF_NULL_FILEHANDLE) {
		errstring = "couldn't open file";
		goto _bailout;
	}

	if (afSetVirtualSampleFormat(file, AF_DEFAULT_TRACK, AF_SAMPFMT_FLOAT, 32)==-1) {
		errstring = "virtual method failed, get newer libaudiofile!";
		goto _bailout;
	}
		
	if (afSetVirtualPCMMapping(file, AF_DEFAULT_TRACK, 1.0, 0.0, -1.0, 1.0)==-1) {
		errstring = "virtual method failed, get newer libaudiofile!";
		goto _bailout; 
	}
	
	buffer_size = 2048*channelCount;

	buffer = ALLOCN(buffer_size, SAMPLE);
	if(buffer==NULL)
		goto _bailout;

	FILTER_AFTER_INIT;

	eofs=channelCount;
	
	for(i=0;i<channelCount;i++) {
		if (!(track[i].buf=sbuf_get(track[i].p))) eofs--;
		track[i].pos=0;
	}

	while(eofs){
		FILTER_CHECK_STOP;
		wbpos=0;
		do{
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
				}
				else
					/* if one track stops before another we have to fill up
					 * with zeroes
					 */
					buffer[wbpos++]=0.0;
		} while ((wbpos<buffer_size) && (eofs));
		frames = wbpos/channelCount;
		if (frames>0) {
			written = afWriteFrames(file, AF_DEFAULT_TRACK, buffer, frames);
			if (written!=frames) {
				failed=1;
				errstring="couldn't write all frames(disk full?)";
				break;
			}
		}
	}

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;
	if (failed==0)
		res=0;
_bailout:
 	afCloseFile(file);
        if(fsetup) afFreeFileSetup(fsetup);
	free(buffer); 
	if (res==-1) FILTER_ERROR_RETURN(errstring); 
	return res;
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
				    //	    FILTERPARAM_HIDDEN, "FIXME",
				    FILTERPARAM_END);

	filterparamdb_add_param_int(filter_paramdb(f), "samplewidth",
				    FILTER_PARAMTYPE_INT, 16,
				    // FILTERPARAM_HIDDEN, "FIXME",
				    FILTERPARAM_END);
	
	filterparamdb_add_param_int(filter_paramdb(f), "compression",
				    FILTER_PARAMTYPE_INT, AF_COMPRESSION_NONE,
				    //FILTERPARAM_HIDDEN, "FIXME",
				    FILTERPARAM_END);
	f->f = write_file_f;

	plugin_set(pl, PLUGIN_DESCRIPTION, "write a file");
	plugin_set(pl, PLUGIN_PIXMAP, "output.png");
	plugin_set(pl, PLUGIN_CATEGORY, "Output");
	plugin_set(pl, PLUGIN_GUI_HELP_PATH, "File_I_O");
	
	filter_register(f, pl);

	return 0;
}
