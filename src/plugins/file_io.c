/*
 * file_io.c
 * $Id: file_io.c,v 1.22 2000/04/07 14:06:50 nold Exp $
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

int wav_read_prepare(filter_node_t *n, const char *filename);
int wav_read_connect(filter_node_t *n, filter_pipe_t *p);
int wav_read_f(filter_node_t *n);
void wav_read_cleanup(filter_node_t *n);

#ifdef HAVE_AUDIOFILE
#include <audiofile.h>
int af_read_prepare(filter_node_t *n, const char *filename);
int af_read_connect(filter_node_t *n, filter_pipe_t *p);
int af_read_f(filter_node_t *n);
void af_read_cleanup(filter_node_t *n);
int af_write_f(filter_node_t *n);
#endif


PLUGIN_SET(file_io, "read_file write_file")


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
			caddr_t		map;
			size_t		size;
			filter_pipe_t 	**p;
			gl_u32		freq;
			gl_u32		bps;
			gl_s32		frames;
			gl_u16		ch;
			gl_u16		block_align;
			gl_u16		bit_width;
			char		*data;
		} wav;
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
#define RWW(node) (RWPRIV(node)->u.wav)
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

	if (!prepare && !f)
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
	    && RWPRIV(n)->rw->cleanup 
	    && RWPRIV(n)->initted)
		RWPRIV(n)->rw->cleanup(n);
	free(RWPRIV(n));
}

/* read methods */
static int read_file_f(filter_node_t *n)
{
	/* require set filename (a selected reader) and
	 * at least one connected output. */
	if (!RWPRIV(n)->initted)
		FILTER_ERROR_RETURN("invalid file");
	if (!filternode_get_output(n, PORTNAME_OUT))
		FILTER_ERROR_RETURN("no outputs");
	return RWPRIV(n)->rw->f(n);
}
static int read_file_connect_out(filter_node_t *n, const char *port,
				 filter_pipe_t *p)
{
	/* no reader -> no filename -> some "defaults".
	 * only allow 2 connections. */
	if (!RWPRIV(n)->rw) {
		if (filternode_nroutputs(n) > 1)
			return -1;
		filterpipe_settype_sample(p, GLAME_DEFAULT_SAMPLERATE,
					  FILTER_PIPEPOS_DEFAULT);
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
	filternode_foreach_output(n, p) {
		if (RWPRIV(n)->rw->connect(n, p) == -1) {
			filternetwork_break_connection(p);
			goto reconnect;
		}
		p->dest->filter->fixup_pipe(p->dest, p);
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

	if (p){
		DPRINTF("Pipe change\n");
		return 0;
	}
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


PLUGIN_DESCRIPTION(read_file, "read a file")
PLUGIN_PIXMAP(read_file, "default.xpm")
int read_file_register()
{
	filter_t *f;
	filter_portdesc_t *p;
	filter_paramdesc_t *d, *pos;
	if (!(f = filter_alloc(read_file_f))
	    || !(p = filter_add_output(f, PORTNAME_OUT, "output channels",
				       FILTER_PORTTYPE_SAMPLE|FILTER_PORTTYPE_AUTOMATIC))
	    || !(pos = filterport_add_param(p, "position", 
	                                    "position of the stream",
	                                    FILTER_PARAMTYPE_FLOAT))
	    || !(d = filter_add_param(f, "filename", "filename",
				    FILTER_PARAMTYPE_STRING)))
		return -1;
	filterparamdesc_float_settype(pos, FILTER_PARAM_FLOATTYPE_POSITION);
	filterparamdesc_string_settype(d, FILTER_PARAM_STRINGTYPE_FILENAME);
	f->init = rw_file_init;
	f->cleanup = rw_file_cleanup;
	f->connect_out = read_file_connect_out;
	f->fixup_param = read_file_fixup_param;
	if (filter_add(f, "read-file", "Generic file read filter") == -1)
		return -1;
	return 0;
}

PLUGIN_DESCRIPTION(write_file, "write a file")
PLUGIN_PIXMAP(write_file, "default.xpm")
int write_file_register()
{
	filter_t *f;
	filter_portdesc_t *p;
	filter_paramdesc_t *d;

	if (!(f = filter_alloc(write_file_f))
	    || !(p = filter_add_input(f, PORTNAME_IN, "input channels",
				       FILTER_PORTTYPE_SAMPLE|FILTER_PORTTYPE_AUTOMATIC))
	    || !(d = filter_add_param(f, "filename", "filename",
				      FILTER_PARAMTYPE_STRING)))
	  return -1;
	filterparamdesc_string_settype(d, FILTER_PARAM_STRINGTYPE_FILENAME);
	f->init = rw_file_init;
	f->cleanup = rw_file_cleanup;
	f->fixup_param = write_file_fixup_param;
	if (filter_add(f, "write-file", "Generic file write filter") == -1)
		return -1;
	return 0;
}

int file_io_register()
{
	INIT_LIST_HEAD(&readers);
	INIT_LIST_HEAD(&writers);

#ifdef HAVE_AUDIOFILE
	add_reader(af_read_prepare, af_read_connect,
		   af_read_f, af_read_cleanup);
	add_writer(af_write_f,"*.wav"); 
#endif
	add_reader(wav_read_prepare, wav_read_connect, 
	           wav_read_f, wav_read_cleanup);

	return 0;
}



/* The actual readers and writers.
 */

typedef int (*wav_chunk_handler_t)(filter_node_t *n, char *tag, char *pos, 
                                   int size);

typedef struct {
	char			*tag;
	wav_chunk_handler_t	handler;
} wav_handlers_t;

#define WAV_FMT_PCM	1

int wav_chunk_ignore(filter_node_t *n, char *tag, char *pos, int size)
{
	
	DPRINTF("WAV chunk %s ignored. Skipping %i bytes.\n", tag, size);
	return size;
}

int wav_read_chunk_head(filter_node_t *n, char *tag, char *pos, int size)
{
	if (strncasecmp(pos, "WAVE", 4)) {
		return -1;	/* RIFF but no WAVE */
	}
	
	return 4;
}

int wav_read_chunk_format(filter_node_t *n, char *tag, char *pos, int size)
{
	if (size < 16) {
		DPRINTF("Illegal chunk size.\n");
		return -1;
	}
	
	/* Only uncompressed PCM handled. Leave all the tricky
	 * stuff to dedicated libs.
	 */
	if (__gl_le16_to_cpup(pos) != WAV_FMT_PCM)
		return -1;
	
	pos += 2;
	RWW(n).ch = __gl_le16_to_cpup(pos);
	pos += 2;
	RWW(n).freq = __gl_le32_to_cpup(pos);
	pos += 4;
	RWW(n).bps = __gl_le32_to_cpup(pos);
	pos += 4;
	RWW(n).block_align = __gl_le16_to_cpup(pos);
	pos += 2;
	RWW(n).bit_width = __gl_le16_to_cpup(pos);
	
	/* Internal limitations */
	if (RWW(n).freq < 2 || !RWW(n).ch) {
		DPRINTF("No channels or frequency unreasonably low.\n");
		return -1;
	}
	switch (RWW(n).block_align/RWW(n).ch) {
		case 1:
		case 2:
			break;	/* supported */
		default:
			DPRINTF("Unsupported width %d.\n", 
					RWW(n).block_align/RWW(n).ch);
			return -1;
	}
	
	return size;
}	

int wav_read_chunk_data(filter_node_t *n, char *tag, char *pos, int size)
{
	if (!RWW(n).ch) {
		DPRINTF("No fmt chunk?\n");
		return -1;
	}

	RWW(n).data = pos;
	RWW(n).frames = size / RWW(n).block_align;
	if (size % RWW(n).block_align) {
		DPRINTF("WAV data not aligned.\n");
		return -1;
	}
	return size;
}

wav_handlers_t wav_read_handlers[] = {
	{ "RIFF", wav_read_chunk_head },
	{ "fmt ", wav_read_chunk_format },
	{ "data", wav_read_chunk_data },
	{ "cue ", wav_chunk_ignore },
	{ "plst", wav_chunk_ignore },
	{ "list", wav_chunk_ignore },
	{ "smpl", wav_chunk_ignore },
	{ "inst", wav_chunk_ignore },
	{ "wavl", wav_chunk_ignore },
	{ NULL, NULL }
};
	

/* Currently we check with zero tolerance. Some of the checks should 
 * perhaps be more lenient and just return 0 instead of -1. Change as
 * necessary. Applies to chunk handlers above as well.
 */
int wav_read_parse(filter_node_t *n, char *from, char* to)
{
	int i, size;
	char *tag;
	wav_chunk_handler_t handler;
	
	RWW(n).ch = 0;

	while (from < to) {
		if (to - from < 8) {
			/* Fail gracefully if all required chunks are present */
			DPRINTF("Premature EOF.\n");
			return RWW(n).data ? 0 : -1;
		}
		for (i=0; (handler=wav_read_handlers[i].handler); i++)
			if (!strncasecmp(from, (tag=wav_read_handlers[i].tag), 
			                 4)) 
				break;

		if (!handler) 
			return -1;
		
		from += 4;
		size = __gl_le32_to_cpup(from);
		from += 4;
		if (from + size > to) {
			DPRINTF("Illegal size in %s chunk.\n", tag);
			return -1;
		}
		if ((size = handler(n, tag, from, size)) == -1) {
			DPRINTF("%s handler failed.\n", tag);
			return -1;
		}
		from += size + (size&1);
			
	}
	return 0;
}				

int wav_read_prepare(filter_node_t *n, const char *filename)
{
	int fd;
	struct stat statbuf;

	fd = open(filename, O_RDONLY);
	if (fd == -1) {
		DPRINTF("%s", strerror(errno));
		return -1;
	}
	if (fstat(fd, &statbuf) == -1) {
		DPRINTF("%s", strerror(errno));
		close(fd);
		return -1;
	}
	RWW(n).size = statbuf.st_size;
	RWW(n).map = mmap(NULL, statbuf.st_size, PROT_READ, MAP_PRIVATE,
	                  fd, 0);
	close(fd);
	if (RWW(n).map == MAP_FAILED) {
		DPRINTF("%s", strerror(errno));
		return -1;
	}

	/* Ugly black magic. Reset values that are abused later on for
	 * state tracking.
	 */
	RWW(n).data = NULL;
	RWW(n).ch = 0;

	if (wav_read_parse(n, RWW(n).map, RWW(n).map + statbuf.st_size)) {
		munmap(RWW(n).map, statbuf.st_size);
		return -1;
	}
	/* All pipes get reconnected via the connect method anyway. */
	if (RWW(n).p)
		free(RWW(n).p);
	RWW(n).p = (filter_pipe_t **)ALLOCN(RWW(n).ch, filter_pipe_t *);
	return 0;
}
	
int wav_read_connect(filter_node_t *n, filter_pipe_t *p)
{
	int i;

	for (i=0; i < RWW(n).ch; i++) {
		if (!RWW(n).p[i]) 
			break;
		if (RWW(n).p[i] == p)
			return 0;
	}
	if (i == RWW(n).ch)
		return -1;

	RWW(n).p[i] = p;
	/* FIXME: While the WAV standard specifies entries for just about 
	 *        everything, there's no information about position. For
	 *        stereo files, left before right is merely convention,
	 *        for quad files even conventions aren't unique anymore.
	 *        For now, mag's algorithm will do. We're fucked anyway.
	 */
	switch (RWW(n).ch) {
		case 1:
			filterpipe_settype_sample(p, RWW(n).freq, 
					          FILTER_PIPEPOS_CENTRE);
			break;
		/* TODO: sth clever... */
		default:
			filterpipe_settype_sample(p, RWW(n).freq, 
					          M_PI/(RWW(n).ch-1)
						  * i + FILTER_PIPEPOS_LEFT);
	}
	return 0;
}
					
void wav_read_cleanup(filter_node_t *n)
{
	free(RWW(n).p);
	if (RWW(n).map)
		munmap(RWW(n).map, RWW(n).size);
}
	
/* TODO: Speed optimization, handle more formats. */
void inline wav_read_convert(filter_buffer_t *buf, int frames, int width, 
		             unsigned int pad, int framesize, char *pos)
{
	SAMPLE *out = sbuf_buf(buf);
/* Sample stub */
#if 0	
	while (frames--) {
		*(out++) = XX2SAMPLE(__gl_leXX_to_cpup(pos) >> pad);
		pos += framesize;
	}
#endif
	switch (width) {
		case 1:
			/* No endianness conversion necessary */
			while (frames--) {
				*(out++) = UCHAR2SAMPLE((*pos) >> pad);
				pos += framesize;
			}
			return;
		case 2:
			while (frames--) {
				*(out++) = SHORT2SAMPLE((__gl_le16_to_cpup(pos))
				                      >> pad);
				pos += framesize;
			}
			return;
		default:
			PANIC("Unsupported width. Should have checked earlier!");
	}
}

int wav_read_f(filter_node_t *n)
{
	filter_buffer_t *buf;
	int bufsize, blksize;
	int align, ch, to_go;
	char *pos = RWW(n).data;
	const int pad = (RWW(n).block_align << 3)/RWW(n).ch - RWW(n).bit_width;
	const int ssize = RWW(n).block_align/RWW(n).ch;
	
	/* Try to buffer approx .5 sec but honour limits. */
	blksize = RWW(n).freq >> 1;

	if (blksize < GLAME_MIN_BUFSIZE)
		blksize = GLAME_MIN_BUFSIZE;
	else if (blksize > GLAME_MAX_BUFSIZE/2)
		blksize = GLAME_MAX_BUFSIZE/2;
	
	align = RWW(n).frames % blksize;
	to_go = RWW(n).frames / blksize;
	
	if (align < GLAME_MIN_BUFSIZE) {
		align += blksize;
		to_go--;
	}
	
	bufsize = align;

	FILTER_AFTER_INIT;
	
	do {
		FILTER_CHECK_STOP;
		
		for (ch = 0; ch < RWW(n).ch; ch++) {
			/* Avoid unnecessary conversion */
			if (!RWW(n).p[ch])
				continue;
			buf = sbuf_make_private(sbuf_alloc(bufsize, n));
			wav_read_convert(buf, bufsize, ssize, pad, 
			                 RWW(n).block_align, pos);
			sbuf_queue(RWW(n).p[ch], buf);
			pos += ssize;
		}
		pos += (bufsize-1)*RWW(n).block_align;
		bufsize = blksize;
	} while(to_go--);
		
	for (ch = 0; ch < RWW(n).ch; ch++)
		sbuf_queue(RWW(n).p[ch], NULL);

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;
	
	FILTER_RETURN;
}

#ifdef HAVE_AUDIOFILE
int af_read_prepare(filter_node_t *n, const char *filename)
{
	if ((RWA(n).file=afOpenFile(filename,"r",NULL))==NULL){ 
		DPRINTF("File not found!\n"); 
		return -1; 
	}
	RWA(n).frameCount=afGetFrameCount(RWA(n).file, AF_DEFAULT_TRACK);
	RWA(n).channelCount = afGetChannels(RWA(n).file, AF_DEFAULT_TRACK);
	afGetSampleFormat(RWA(n).file, AF_DEFAULT_TRACK, &(RWA(n).sampleFormat), &(RWA(n).sampleWidth));
	RWA(n).frameSize = afGetFrameSize(RWA(n).file, AF_DEFAULT_TRACK, 1);
	RWA(n).sampleRate = (int)afGetRate(RWA(n).file, AF_DEFAULT_TRACK);
	if ((RWA(n).sampleFormat != AF_SAMPFMT_TWOSCOMP &&
	     RWA(n).sampleFormat != AF_SAMPFMT_UNSIGNED) || 
	    (RWA(n).sampleWidth != 8 && RWA(n).sampleWidth != 16)) {
		DPRINTF("Format not supported!\n");
		return -1;
	}
	if (RWA(n).sampleWidth == 8 && 
	    RWA(n).sampleFormat == AF_SAMPFMT_TWOSCOMP && 
	    afGetFileFormat(RWA(n).file, NULL) == AF_FILE_WAVE) {
		RWA(n).sampleFormat = AF_SAMPFMT_UNSIGNED;
		DPRINTF("Kludge! Audiofile reports signed 8bit WAV, "
		        "overriding to unsigned.\n");
	}
		
			

	if (RWA(n).buffer) free(RWA(n).buffer);
	if ((RWA(n).buffer=(short int*)malloc(GLAME_WBUFSIZE*RWA(n).frameSize))==NULL){
		DPRINTF("Couldn't allocate buffer\n");
		return -1;
	}
	RWA(n).cbuffer=(char *)RWA(n).buffer;
	if (RWA(n).track) free(RWA(n).track);
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

int af_read_connect(filter_node_t *n, filter_pipe_t *p)
{
	int i;
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

int af_read_f(filter_node_t *n)
{
	int frames,i,j;
	filter_pipe_t *p_out;
	SAMPLE *s0, *s1;
	short *b;
	int fcnt, cnt;

	/* seek to start of audiofile */
	afSeekFrame(RWA(n).file, AF_DEFAULT_TRACK, 0);
	fcnt = RWA(n).frameCount;

	FILTER_AFTER_INIT;

	while(fcnt){
		FILTER_CHECK_STOP;
		if (!(frames=afReadFrames(RWA(n).file, AF_DEFAULT_TRACK, 
					  RWA(n).buffer,
					  MIN(GLAME_WBUFSIZE, fcnt))))
			break;
		fcnt-=frames;
		for (i=0; i < RWA(n).channelCount; i++){
			RWA(n).track[i].buf =
				sbuf_make_private(sbuf_alloc(frames,n));
			RWA(n).track[i].pos = 0;
		}
		i=0;
		/* XXX: Dangerous but for now happens to work! */
		switch (RWA(n).sampleWidth | RWA(n).sampleFormat) {
		case 16 | AF_SAMPFMT_TWOSCOMP:
			switch (RWA(n).channelCount) {
			case 2:
				/* highly optimized default (wav) case [richi] */
				cnt = frames;
				s0 = &sbuf_buf(RWA(n).track[0].buf)[RWA(n).track[0].pos];
				s1 = &sbuf_buf(RWA(n).track[1].buf)[RWA(n).track[1].pos];
				b = RWA(n).buffer;
				for (; (cnt & 3)>0; cnt--) {
					*(s0++) = SHORT2SAMPLE(*(b++));
					*(s1++) = SHORT2SAMPLE(*(b++));
				}
				for (; cnt>0; cnt-=4) {
					*(s0++) = SHORT2SAMPLE(*(b++));
					*(s1++) = SHORT2SAMPLE(*(b++));
					*(s0++) = SHORT2SAMPLE(*(b++));
					*(s1++) = SHORT2SAMPLE(*(b++));
					*(s0++) = SHORT2SAMPLE(*(b++));
					*(s1++) = SHORT2SAMPLE(*(b++));
					*(s0++) = SHORT2SAMPLE(*(b++));
					*(s1++) = SHORT2SAMPLE(*(b++));
				}
				RWA(n).track[0].pos += frames;
				RWA(n).track[1].pos += frames;
				break;
			default:
				while (i < frames*RWA(n).channelCount)
					for (j=0; j < RWA(n).channelCount; j++)
						sbuf_buf(RWA(n).track[j].buf)[RWA(n).track[j].pos++] =
							SHORT2SAMPLE(RWA(n).buffer[i++]);
				break;
			}
			break;
		case 8 | AF_SAMPFMT_TWOSCOMP:
			while (i < frames*RWA(n).channelCount)
				for (j=0; j < RWA(n).channelCount; j++)
					sbuf_buf(RWA(n).track[j].buf)[RWA(n).track[j].pos++] = 
						CHAR2SAMPLE(RWA(n).cbuffer[i++]);
			break;
		case 16 | AF_SAMPFMT_UNSIGNED:
			while (i < frames*RWA(n).channelCount)
				for (j=0; j < RWA(n).channelCount; j++)
					sbuf_buf(RWA(n).track[j].buf)[RWA(n).track[j].pos++] =
						USHORT2SAMPLE(RWA(n).buffer[i++]);
			break;
		case 8 | AF_SAMPFMT_UNSIGNED:
			while (i < frames*RWA(n).channelCount)
				for (j=0; j < RWA(n).channelCount; j++)
					sbuf_buf(RWA(n).track[j].buf)[RWA(n).track[j].pos++] =
						UCHAR2SAMPLE(RWA(n).cbuffer[i++]);
			break;
		default:
			PANIC("Unsupported sample format.");
		}
		for (i=0; i < RWA(n).channelCount; i++)
			sbuf_queue(RWA(n).track[i].p, RWA(n).track[i].buf);
	}
	filternode_foreach_output(n, p_out) 
		sbuf_queue(p_out, NULL);

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	return 0;
}

void af_read_cleanup(filter_node_t *n)
{
	free(RWA(n).buffer);
	free(RWA(n).track);
	afCloseFile(RWA(n).file);	
}

int af_write_f(filter_node_t *n)
{
	filter_pipe_t *in;
	filter_param_t *param;
	char *filename=NULL;
	int res=-1;
	int eofs,bufsiz,wbpos;
	int i,iat,iass;
	
	RWA(n).channelCount=filternode_nrinputs(n);

	if ((param=filternode_get_param(n,"filename")))
		filename=filterparam_val_string(param);
	else
		FILTER_ERROR_RETURN("no filename");

	if (RWA(n).channelCount==0)
		FILTER_ERROR_RETURN("no inputs");
	if (!(RWA(n).track=ALLOCN(RWA(n).channelCount,track_t)))
		FILTER_ERROR_RETURN("no memory");
	
	iass=0;
	filternode_foreach_input(n, in) {
		for(iat=0;iat<iass && FILTER_SAMPLEPIPE_MORE_LEFT(RWA(n).track[iat].p,in);iat++);
		for(i=iass;i>iat;i--)
			RWA(n).track[i]=RWA(n).track[i-1];
		RWA(n).track[iat].p=in;
		if(iass==0)
			RWA(n).sampleRate=filterpipe_sample_rate(in);
		else 
			if (filterpipe_sample_rate(in)!=RWA(n).sampleRate)
				FILTER_ERROR_RETURN("inconsistent samplerates");
		iass++;
	}
	
	RWA(n).fsetup=afNewFileSetup();
	afInitFileFormat(RWA(n).fsetup,AF_FILE_WAVE);
	afInitChannels(RWA(n).fsetup, AF_DEFAULT_TRACK, RWA(n).channelCount);
	afInitSampleFormat(RWA(n).fsetup, AF_DEFAULT_TRACK, AF_SAMPFMT_TWOSCOMP, 16);
	afInitRate(RWA(n).fsetup, AF_DEFAULT_TRACK,RWA(n).sampleRate);
	RWA(n).file=afOpenFile(filename, "w", RWA(n).fsetup);

	if (RWA(n).file==AF_NULL_FILEHANDLE)
		goto _bailout;
	

	bufsiz=GLAME_WBUFSIZE*RWA(n).channelCount;
	if ((RWA(n).buffer=(short*)malloc(bufsiz*sizeof(short)))==NULL)
		goto _bailout;
	
	
	FILTER_AFTER_INIT;

	eofs=RWA(n).channelCount;
	
	for(i=0;i<RWA(n).channelCount;i++) {
		if (!(RWA(n).track[i].buf=sbuf_get(RWA(n).track[i].p))) eofs--;
		RWA(n).track[i].pos=0;
	}

	while(eofs){
		FILTER_CHECK_STOP;
		wbpos=0;
		do{
			/* write one interleaved frame to buffer */
			for(i=0;i<RWA(n).channelCount;i++)
				if (RWA(n).track[i].buf){
					RWA(n).buffer[wbpos++]=SAMPLE2SHORT(sbuf_buf(RWA(n).track[i].buf)[RWA(n).track[i].pos++]);
					/* Check for end of buffer */
					if(RWA(n).track[i].pos==sbuf_size(RWA(n).track[i].buf)){
						sbuf_unref(RWA(n).track[i].buf);
						if (!(RWA(n).track[i].buf=sbuf_get(RWA(n).track[i].p))) eofs--;
						RWA(n).track[i].pos=0;
					}
				}
				else
					/* if one track stops before another we have to fill up
					 * with zeroes
					 */
					RWA(n).buffer[wbpos++]=0;
		} while(wbpos<bufsiz);
		afWriteFrames(RWA(n).file, AF_DEFAULT_TRACK, RWA(n).buffer,wbpos/RWA(n).channelCount);
	}
		
	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;
	res=0;
_bailout:
 	afCloseFile(RWA(n).file);
        if(RWA(n).fsetup) afFreeFileSetup(RWA(n).fsetup);
	free(RWA(n).buffer);
	free(RWA(n).track);
	if (res==-1) FILTER_ERROR_RETURN("some error occured"); 
	return res;
}

#endif
