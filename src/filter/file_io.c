/*
 * file_io.c
 * $Id: file_io.c,v 1.12 2000/02/23 12:31:12 richi Exp $
 *
 * Copyright (C) 1999, 2000 Alexander Ehlert, Richard Guenther
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
 * channel of the file for the pipe. Remixing is not required, as is
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
 * a writer should register itself with a regular expression of filenames
 * it wants to handle.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <sys/time.h>
#include <sys/types.h>
#include <regex.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "filter.h"
#include "util.h"

#ifdef HAVE_AUDIOFILE
#include <audiofile.h>
int af_read_prepare(filter_node_t *n, const char *filename);
int af_read_connect(filter_node_t *n, filter_pipe_t *p);
int af_read_f(filter_node_t *n);
void af_read_cleanup(filter_node_t *n);
/* int af_write_f(filter_node_t *n); */
#endif


typedef struct {
	struct list_head list;
	int (*prepare)(filter_node_t *, const char *);
	int (*connect)(filter_node_t *, filter_pipe_t *);
	int (*f)(filter_node_t *);
	void (*cleanup)(filter_node_t *);
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
			int dummy;
		} dummy;
#ifdef HAVE_AUDIOFILE
		struct {
			AFfilehandle    file;
			AFframecount    frameCount;
		        AFfilesetup     fsetup;
			int             sampleFormat,sampleWidth;
			int             channelCount,frameSize;
			int		sampleRate;
		        int             format;
			track_t         *track;
			short		*buffer;
			char		*cbuffer;
		} audiofile;
#endif
	} u;
} rw_private_t;
#define RWPRIV(node) ((rw_private_t *)((node)->private))
#define RWA(node) (RWPRIV(node)->u.audiofile)

/* the readers & the writers list */
static struct list_head readers;
static struct list_head writers;


static rw_t *add_rw(int (*prepare)(filter_node_t *, const char *),
		    int (*connect)(filter_node_t *, filter_pipe_t *),
		    int (*f)(filter_node_t *),
		    void (*cleanup)(filter_node_t *),
		    const char *regexp)
{
	rw_t *rw;

	if (!prepare || !f)
		return NULL;
	if (!(rw = ALLOC(rw_t)))
		return NULL;
	INIT_LIST_HEAD(&rw->list);
	rw->prepare = prepare;
	rw->connect = connect;
	rw->f = f;
	rw->cleanup = cleanup;
	if (regexp)
		rw->regexp = strdup(regexp);

	return rw;
}
static int add_reader(int (*prepare)(filter_node_t *, const char *),
		      int (*connect)(filter_node_t *, filter_pipe_t *),
		      int (*f)(filter_node_t *),
		      void (*cleanup)(filter_node_t *))
{
	rw_t *rw;

	if (!(rw = add_rw(prepare, connect, f, cleanup, NULL)))
	        return -1;
	list_add(&rw->list, &readers);

	return 0;
}
static int add_writer(int (*f)(filter_node_t *), const char *regexp)
{
	rw_t *rw;

	if (!(rw = add_rw(NULL, NULL, f, NULL, regexp)))
	        return -1;
	list_add(&rw->list, &writers);

	return 0;
}


/* generic read&write methods */
static int rw_file_init(filter_node_t *n)
{
	rw_private_t *p;

	if (!(p = ALLOC(rw_private_t)))
		return -1;
	n->private = p;
	return 0;
}
static void rw_file_cleanup(filter_node_t *n)
{
	if (RWPRIV(n)->rw
	    && RWPRIV(n)->rw->cleanup)
		RWPRIV(n)->rw->cleanup(n);
	free(RWPRIV(n));
}

/* read methods */
static int read_file_f(filter_node_t *n)
{
	/* require set filename (a selected reader) and
	 * at least one connected output. */
	if (!RWPRIV(n)->initted)
		return -1;
	if (!filternode_get_output(n, PORTNAME_OUT))
		return -1;
	return RWPRIV(n)->rw->f(n);
}
static int read_file_connect_out(filter_node_t *n, const char *port,
				 filter_pipe_t *p)
{
	/* no reader -> no filename -> some "defaults" */
	if (!RWPRIV(n)->rw) {
		filterpipe_settype_sample(p, 44100, FILTER_PIPEPOS_DEFAULT);
		return 0;
	}

	/* pass request to readers prepare, it can reject the
	 * connection, but not the file here. */
	return RWPRIV(n)->rw->connect(n, p);
}
static int read_file_fixup_param(filter_node_t *n, filter_pipe_t *p,
				 const char *name, filter_param_t *param)
{
	rw_t *r;

	/* only pipe param change (position)? */
	if (p && RWPRIV(n)->rw) {
		if (RWPRIV(n)->rw->connect(n, p) == -1)
			filternetwork_break_connection(p);
		return 0;
	
        /* filename change! */
	} else {
		/* check actual reader */
		if (RWPRIV(n)->rw) {
			/* cleanup previous stuff */
			if (RWPRIV(n)->initted
			    && RWPRIV(n)->rw->cleanup)
				RWPRIV(n)->rw->cleanup(n);
			RWPRIV(n)->initted = 0;

			/* try same rw again */
			if (RWPRIV(n)->rw->prepare(n, filterparam_val_string(param)) != -1) {
				RWPRIV(n)->initted = 1;
				goto reconnect;
			}
		}

		RWPRIV(n)->rw = NULL;
		RWPRIV(n)->initted = 0;

		/* search for applicable reader */
		list_foreach(&readers, rw_t, list, r) {
			if (r->prepare(n, filterparam_val_string(param)) != -1) {
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
	filternode_foreach_output(n, p)
		if (RWPRIV(n)->rw->connect(n, p) == -1){
			filternetwork_break_connection(p);
			goto reconnect;
		}

	return 0;
}

/* write methods */
static int write_file_f(filter_node_t *n)
{
	/* require set filename, a selected writer and
	 * at least one connected input. */
	if (!RWPRIV(n)->initted)
		return -1;
	if (!filternode_get_input(n, PORTNAME_IN))
		return -1;
	return RWPRIV(n)->rw->f(n);
}
static int write_file_fixup_param(filter_node_t *n, filter_pipe_t *p,
				  const char *name, filter_param_t *param)
{
	regex_t rx;
	rw_t *w;

        /* only filename change possible in writer. */
	RWPRIV(n)->initted = 0;
	RWPRIV(n)->rw = NULL;

	/* find applicable writer */
	list_foreach(&writers, rw_t, list, w) {
	        if (regcomp(&rx, w->regexp, REG_EXTENDED|REG_NOSUB) == -1)
			continue;
		if (regexec(&rx, filterparam_val_string(param), 0,
			    NULL, 0) == 0) {
			regfree(&rx);
			RWPRIV(n)->rw = w;
			RWPRIV(n)->initted = 1;
			return 0;
		}
		regfree(&rx);
	}

	return -1;
}



int file_io_register()
{
	filter_t *f;
	filter_portdesc_t *p;

	INIT_LIST_HEAD(&readers);
	INIT_LIST_HEAD(&writers);

	if (!(f = filter_alloc("read_file", "Generic file read filter",
			       read_file_f))
	    || !(p = filter_add_output(f, PORTNAME_OUT, "output channels",
				       FILTER_PORTTYPE_SAMPLE|FILTER_PORTTYPE_AUTOMATIC))
	    || !filterport_add_param(p, "position", "position of the stream",
				     FILTER_PARAMTYPE_FLOAT)
	    || !filter_add_param(f, "filename", "filename",
				 FILTER_PARAMTYPE_STRING))
		return -1;
	f->init = rw_file_init;
	f->cleanup = rw_file_cleanup;
	f->connect_out = read_file_connect_out;
	f->fixup_param = read_file_fixup_param;
	if (filter_add(f) == -1)
		return -1;

	if (!(f = filter_alloc("write_file", "Generic file write filter",
			       write_file_f))
	    || !(p = filter_add_input(f, PORTNAME_IN, "input channels",
				       FILTER_PORTTYPE_SAMPLE|FILTER_PORTTYPE_AUTOMATIC))
	    || !filter_add_param(f, "filename", "filename",
				 FILTER_PARAMTYPE_STRING))
		return -1;
	f->init = rw_file_init;
	f->cleanup = rw_file_cleanup;
	f->fixup_param = write_file_fixup_param;
	if (filter_add(f) == -1)
		return -1;

#ifdef HAVE_AUDIOFILE
	add_reader(af_read_prepare, af_read_connect,
		   af_read_f, af_read_cleanup);
	/* add_writer(af_write_f, ".*(\\.wav|\\.au)$"); */
#endif

	return 0;
}



/* The actual readers and writers.
 */
#ifdef HAVE_AUDIOFILE
int af_read_prepare(filter_node_t *n, const char *filename)
{
	DPRINTF("Read %s\n",filename);
	if ((RWA(n).file=afOpenFile(filename,"r",NULL))==NULL){ 
		DPRINTF("File not found!\n"); 
		return -1; 
	}
	RWA(n).frameCount=afGetFrameCount(RWA(n).file, AF_DEFAULT_TRACK);
	RWA(n).channelCount = afGetChannels(RWA(n).file, AF_DEFAULT_TRACK);
	afGetSampleFormat(RWA(n).file, AF_DEFAULT_TRACK, &(RWA(n).sampleFormat), &(RWA(n).sampleWidth));
	RWA(n).frameSize = afGetFrameSize(RWA(n).file, AF_DEFAULT_TRACK, 1);
	RWA(n).sampleRate = (int)afGetRate(RWA(n).file, AF_DEFAULT_TRACK);
	if ((RWA(n).sampleFormat != AF_SAMPFMT_TWOSCOMP)){
		DPRINTF("Format not supported!\n");
		return -1;
	}
	if ((RWA(n).buffer=(short int*)malloc(GLAME_WBUFSIZE*RWA(n).frameSize))==NULL){
		DPRINTF("Couldn't allocate buffer\n");
		return -1;
	}
	RWA(n).cbuffer=(char *)RWA(n).buffer;
	if (!(RWA(n).track=ALLOCN(RWA(n).channelCount,track_t))){
		DPRINTF("Couldn't allocate track buffer\n");
		return -1;
	}
	return 0;
}

#if 0
int af_write_prepare(filter_node_t *n, const char *filename)
{
	DPRINTF("Write %s\n",filename);
	fsetup=afNewFileSetup();
        afInitFileFormat(fsetup,format);
	afInitChannels(fsetup, AF_DEFAULT_TRACK, channelCount);
	afInitSampleFormat(fsetup, AF_DEFAULT_TRACK, AF_SAMPFMT_TWOSCOMP, 16);
	file=afOpenFile(filename, "w", fsetup);
	if (file==AF_NULL_FILEHANDLE){
		DPRINTF("couldn't open %s\n",filename);
		goto _bailout;
	}
}
#endif

int af_read_connect(filter_node_t *n, filter_pipe_t *p)
{
	int i;
	for(i=0;(i<RWA(n).channelCount) && (RWA(n).track[i].mapped);i++);
	if (i>=RWA(n).channelCount){
		/* Check if track is already mapped ?!
		 * Would be strange, but ... FIXME
		 * - nope, not strange, connect gets called for each
		 *   already connected pipes at parameter change, too!
		 *   (as for just pipe parameter change)
		 * - you should fixup, i.e. re-route perhaps, reject, whatever
		 *   in this case
                 */
		for(i=0;i<RWA(n).channelCount;i++)
			if ((RWA(n).track[i].mapped) && RWA(n).track[i].p==p)
				return 0; /* FIXME */
		
		/* Otherwise error */
		return -1;
	} else {
		/* Moah! what is this? does libaudiofile not provide
		 * some "direct" information on position??
		 */
		filterpipe_settype_sample(p,RWA(n).sampleRate,
			(M_PI/(RWA(n).channelCount-1))*i+FILTER_PIPEPOS_LEFT);
		RWA(n).track[i].p=p;
		RWA(n).track[i].mapped=1;
	}	
	
	return 0;	
}
int af_write_connect(filter_node_t *n, filter_pipe_t *p)
{
	track_t *track;
	int i;
	/* Connect just collects all incoming connections
	 * mapping is done later
	 */
	
	if (!RWA(n).track){
		/* First connection to be opened */
		if (!(RWA(n).track=ALLOC(track_t))){
			DPRINTF("Couldn't allocate track buffer\n");
			return -1;
		}
		RWA(n).channelCount=1;
		RWA(n).track[0].p=p;
	} else {
		/* Connection already collected ? */
		for(i=0;i<RWA(n).channelCount;i++)
			if (RWA(n).track[i].p==p) return 0;
		if (!(RWA(n).track=ALLOCN(i,track_t))){
			DPRINTF("Couldn't allocate track buffer\n");
			return -1;
		}
		/* I know it's not nice... */
		memcpy(track,RWA(n).track,RWA(n).channelCount*sizeof(track_t));
		free(RWA(n).track);
		RWA(n).track=track;
		RWA(n).channelCount++;
		RWA(n).track[i].p=p;
	}
	return 0;
}


int af_read_f(filter_node_t *n)
{
	int frames,i,j;
	filter_pipe_t *p_out;

	FILTER_AFTER_INIT;

	while(RWA(n).frameCount){
		FILTER_CHECK_STOP;
		if (!(frames=afReadFrames(RWA(n).file, AF_DEFAULT_TRACK, RWA(n).buffer,
					  MIN(GLAME_WBUFSIZE,RWA(n).frameCount))))
			break;
		RWA(n).frameCount-=frames;
		for(i=0;i<RWA(n).channelCount;i++){
			RWA(n).track[i].buf=sbuf_alloc(frames,n);
			RWA(n).track[i].buf=sbuf_make_private(RWA(n).track[i].buf);
			RWA(n).track[i].pos=0;
		}
		i=0;
		while(i<frames*RWA(n).channelCount){
			for(j=0;j<RWA(n).channelCount;j++){
				switch(RWA(n).sampleWidth) {
				case 16 :
					sbuf_buf(RWA(n).track[j].buf)[RWA(n).track[j].pos++]=SHORT2SAMPLE(RWA(n).buffer[i++]);
					break;
				case  8 :
					sbuf_buf(RWA(n).track[j].buf)[RWA(n).track[j].pos++]=CHAR2SAMPLE(RWA(n).cbuffer[i++]);
					break;
				}
			}
		}
		for(i=0;i<RWA(n).channelCount;i++)
			sbuf_queue(RWA(n).track[i].p,RWA(n).track[i].buf);
	}
	filternode_foreach_output(n,p_out) 
		sbuf_queue(p_out,NULL);

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	return 0;
}

void af_read_cleanup(filter_node_t *n)
{
	if (!RWPRIV(n)->initted)
		return;
	free(RWA(n).buffer);
	free(RWA(n).track);
	afCloseFile(RWA(n).file);	
}
#endif



/* old write_file code follows */
#if 0
static int write_file_f(filter_node_t *n)
{
	filter_pipe_t *left,*right;
	filter_buffer_t *lbuf,*rbuf;
	filter_param_t *param;
	int sampleFormat,sampleWidth,channelCount,frameSize;
	AFfilehandle    file=AF_NULL_FILEHANDLE;
	char *filename=NULL;
	int format;
	AFfilesetup	fsetup;
	short	*wbuf=NULL;
	int res=-1;
	int wbpos,framesWritten,lpos,rpos;
	
	DPRINTF("write-file started!\n");

	left = filternode_get_input(n, PORTNAME_LEFT_IN);
	right = filternode_get_input(n, PORTNAME_RIGHT_IN);

	channelCount=0;
	
	if(left)  channelCount++;
	if(right){
		if(!left){ 
			left=right;
			right=NULL;
		}
		channelCount++;
	}
	if(channelCount==0) {
		DPRINTF("No input channels\n");
		goto _bailout;
	} else DPRINTF("channelCount=%d\n",channelCount);

	if ((param = filternode_get_param(n, "ext"))) {
		if(strcmp(param->val.string,"wav")==0)
			format=AF_FILE_WAVE;
		else {
			DPRINTF("unknown fileformat!\n");
			goto _bailout;
		}
	} else format=AF_FILE_WAVE;

	if ((param = filternode_get_param(n, "filename")))
			filename=strdup(param->val.string);
	else {
		DPRINTF("missing filename!\n");
		goto _bailout;
	}
	fsetup=afNewFileSetup();
	afInitFileFormat(fsetup,format);
	afInitChannels(fsetup, AF_DEFAULT_TRACK, channelCount);
	afInitSampleFormat(fsetup, AF_DEFAULT_TRACK, AF_SAMPFMT_TWOSCOMP, 16);
	file=afOpenFile(filename, "w", fsetup);
	if (file==AF_NULL_FILEHANDLE){
		DPRINTF("couldn't open %s\n",filename);
		goto _bailout;
	}

	if ((wbuf=(short*)malloc(GLAME_WBUFSIZE*sizeof(short)))==NULL){
		DPRINTF("couldn't allocate writebuffer!\n");
		goto _bailout;
	}
	
	FILTER_AFTER_INIT;

	lbuf=sbuf_get(left);
	if(right) rbuf=sbuf_get(right);
	else rbuf=NULL;

	do {
		while (wbpos < GLAME_WBUFSIZE && (lbuf || rbuf)) {
	               if (lbuf) 
			       wbuf[wbpos++] = SAMPLE2SHORT(sbuf_buf(lbuf)[lpos++]);
	               else wbuf[wbpos++] = 0;
		       if (right) { 
			       if (rbuf) 
				       wbuf[wbpos++] = SAMPLE2SHORT(sbuf_buf(rbuf)[rpos++]); 
			       else wbuf[wbpos++] = 0; 
		       }
		       if(lpos==sbuf_size(lbuf)){
			       sbuf_unref(lbuf);
			       lbuf=sbuf_get(left);
			       lpos=0;
		       }
		       if ( (right) && (rpos==sbuf_size(rbuf))){
			       sbuf_unref(rbuf);
			       rbuf=sbuf_get(right);
			       rpos=0;
		       }
		}
		if((wbpos%channelCount)==1) DPRINTF("Oergs\n");
		framesWritten = afWriteFrames(file, AF_DEFAULT_TRACK, wbuf, wbpos/channelCount);
		wbpos=0;
	} while (lbuf || (right && rbuf));

	afCloseFile(file);
	afFreeFileSetup(fsetup);

	res=0;
_bailout:
	if(filename) free(filename);
	if(wbuf) free(wbuf);
	if (res==-1) {
		if (file!=AF_NULL_FILEHANDLE) afCloseFile(file);
	        FILTER_AFTER_INIT;
	}
	FILTER_BEFORE_CLEANUP;
	return res;
}
#endif
