/*
 * file_io.c
 * $Id: file_io.c,v 1.60 2001/08/11 15:15:53 richi Exp $
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


int wav_read_prepare(filter_t *n, const char *filename);
int wav_read_connect(filter_t *n, filter_pipe_t *p);
int wav_read_f(filter_t *n);
void wav_read_cleanup(filter_t *n);

#ifdef HAVE_AUDIOFILE
#include <audiofile.h>
int af_read_prepare(filter_t *n, const char *filename);
int af_read_connect(filter_t *n, filter_pipe_t *p);
int af_read_f(filter_t *n);
void af_read_cleanup(filter_t *n);
int af_write_f(filter_t *n);
#endif

#ifdef HAVE_LAME
#ifdef HAVE_LAME_H
#include <lame.h>
#elif defined HAVE_LAME_LAME_H
#include <lame/lame.h>
#endif
#include "pthread.h"
/* libmp3lame is not threadsafe */
pthread_mutex_t lamelock = PTHREAD_MUTEX_INITIALIZER;

int lame_read_prepare(filter_t *n, const char *filename);
int lame_read_connect(filter_t *n, filter_pipe_t *p);
int lame_read_f(filter_t *n);
void lame_read_cleanup(filter_t *n);
#endif

PLUGIN_SET(file_io, "read_file write_file")


typedef struct {
	struct list_head list;
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
#ifdef HAVE_LAME
		struct {
			FILE			*infile;
			off_t			start;
			lame_global_flags	*lgflags;
			mp3data_struct		mp3data;
			track_t			*track;
		} lame;
#endif
	} u;
} rw_private_t;
#define RWPRIV(node) ((rw_private_t *)((node)->priv))
#define RWW(node) (RWPRIV(node)->u.wav)
#define RWA(node) (RWPRIV(node)->u.audiofile)
#define RWM(node) (RWPRIV(node)->u.lame)

/* the readers & the writers list */
static struct list_head readers;
static struct list_head writers;


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
	INIT_LIST_HEAD(&rw->list);
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
	list_add(&rw->list, &readers);
	return 0;
}
static int add_writer(int (*f)(filter_t *), const char *regexp)
{
	rw_t *rw;

	if (!(rw = add_rw(NULL, NULL, f, NULL, regexp)))
	        return -1;
	list_add(&rw->list, &writers);
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
		list_foreach(&readers, rw_t, list, r) {
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

/* write methods */
static int write_file_f(filter_t *n)
{
	/* require set filename, a selected writer and
	 * at least one connected input. */
	if (!RWPRIV(n)->initted)
		return -1;
	if (!filterport_get_pipe(filterportdb_get_port(filter_portdb(n), PORTNAME_IN)))
		return -1;
	return RWPRIV(n)->rw->f(n);
}
static int write_file_connect_in(filter_port_t *port, filter_pipe_t *p)
{
	/* So why is there no write_file_connect_in?? Do we really
	 * support any number of inputs? Seems all is _f() time... */
	return 0;
}

static int write_file_setup_param(filter_param_t *param, const void *val) 
{
	filter_t *n = filterparam_filter(param);
	regex_t rx;
	rw_t *w;

        /* only filename change possible in writer. */
	RWPRIV(n)->initted = 0;
	RWPRIV(n)->rw = NULL;

	/* no filename - no writer. */
	if(*(const char**)val==NULL)
		return -1;

	/* find applicable writer */
	list_foreach(&writers, rw_t, list, w) {
	        if (regcomp(&rx, w->regexp, REG_EXTENDED|REG_NOSUB) == -1)
			continue;
		if (regexec(&rx, *(const char **)val, 0, NULL, 0) == 0) {
			regfree(&rx);
			RWPRIV(n)->rw = w;
			RWPRIV(n)->initted = 1;
			return 0;
		}
		regfree(&rx);
	}
	return -1;
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

int write_file_register(plugin_t *pl)
{
	filter_t *f;
	filter_port_t *in;
	filter_param_t *param;

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
	param->set = write_file_setup_param;

	f->f = write_file_f;
	f->init = rw_file_init;

	plugin_set(pl, PLUGIN_DESCRIPTION, "write a file");
	plugin_set(pl, PLUGIN_PIXMAP, "output.png");
	plugin_set(pl, PLUGIN_CATEGORY, "Output");
	plugin_set(pl, PLUGIN_GUI_HELP_PATH, "File_I_O");
	
	filter_register(f, pl);

	return 0;
}

int file_io_register(plugin_t *p)
{
	INIT_LIST_HEAD(&readers);
	INIT_LIST_HEAD(&writers);
#ifdef HAVE_LAME
	add_reader(lame_read_prepare, lame_read_connect,
		   lame_read_f, lame_read_cleanup);
#endif
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

typedef gl_s32 (*wav_chunk_handler_t)(filter_t *n, char *tag, char *pos, 
                                      gl_s32 size);

typedef struct {
	char			*tag;
	wav_chunk_handler_t	handler;
} wav_handlers_t;

#define WAV_FMT_PCM	1

gl_s32 wav_chunk_ignore(filter_t *n, char *tag, char *pos, gl_s32 size)
{
	
	DPRINTF("WAV chunk %s ignored. Skipping %i bytes.\n", tag, size);
	return size;
}

gl_s32 wav_read_chunk_head(filter_t *n, char *tag, char *pos, gl_s32 size)
{
	if (strncasecmp(pos, "WAVE", 4)) {
		return -1;	/* RIFF but no WAVE */
	}
	
	return 4;
}

gl_s32 wav_read_chunk_format(filter_t *n, char *tag, char *pos, gl_s32 size)
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

gl_s32 wav_read_chunk_data(filter_t *n, char *tag, char *pos, gl_s32 size)
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
int wav_read_parse(filter_t *n, char *from, char* to)
{
	int i;
	char *tag;
	gl_s32 size;
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
		if (size < 0 || from + size > to) {
			DPRINTF("Illegal size in %s chunk (real: %ld, "
			        "adv: %ld).\n", tag, to-from, size);
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

int wav_read_prepare(filter_t *n, const char *filename)
{
	int fd;
	struct stat statbuf;
	filter_param_t *fparam;
	char info[255];

	fparam = filterparamdb_get_param(filter_paramdb(n), "filename");

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
	RWW(n).map = (char *)mmap(NULL, statbuf.st_size, PROT_READ,
				  MAP_PRIVATE, fd, 0);
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

	/* Now update fparam properties */
	sprintf(info,"4 -1"); /* wav version=unknown */
	filterparam_set_property(fparam, "#format", info);

	sprintf(info,"%d Hz",RWW(n).freq);
	filterparam_set_property(fparam, "#samplerate", info);

	sprintf(info,"%d bit",RWW(n).bit_width);
	filterparam_set_property(fparam, "#quality", info);

	sprintf(info,"%d",RWW(n).frames);
	filterparam_set_property(fparam, "#framecount", info);
	
	sprintf(info,"%d",RWW(n).ch);
	filterparam_set_property(fparam, "#channels", info);

	return 0;
}
	
int wav_read_connect(filter_t *n, filter_pipe_t *p)
{
	int i, deleted = 1;
	filter_port_t	*outp;
	filter_pipe_t	*out;
	
	outp = filterportdb_get_port(filter_portdb(n), PORTNAME_OUT);
	filterport_foreach_pipe(outp, out) {
		if (p == out)
			deleted = 0;
	}
	
	if (deleted == 1) {
		for(i=0; i < RWW(n).ch; i++)
			if ( RWW(n).p[i] == p ) {
				RWW(n).p[i] = NULL;
				return 0;
			}
	}
		
	/* Find either a free slot to put the pipe into or
	 * an already registered pipe in which case we just
	 * return success. */
	for (i=0; i < RWW(n).ch; i++) {
		if (!RWW(n).p[i]) 
			break;
		if (RWW(n).p[i] == p)
			return 0;
	}
	/* No free slots? */
	if (i == RWW(n).ch)
		return -1;
	/* Register the pipe. */
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
					
void wav_read_cleanup(filter_t *n)
{
	free(RWW(n).p);
	if (RWW(n).map)
		munmap(RWW(n).map, RWW(n).size);
	memset(&(RWPRIV(n)->u), 0, sizeof(RWPRIV(n)->u));
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

int wav_read_f(filter_t *n)
{
	filter_buffer_t *buf;
	filter_param_t *pos_param;
	int bufsize, blksize;
	int align, ch, to_go, done;
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
	
	if (align < GLAME_MIN_BUFSIZE && to_go) {
		align += blksize;
		to_go--;
	}
	
	bufsize = align;
	
	FILTER_AFTER_INIT;
	pos_param = filterparamdb_get_param(filter_paramdb(n), FILTERPARAM_LABEL_POS);
        filterparam_val_set_pos(pos_param, 0);
	done = 0;
	do {
		FILTER_CHECK_STOP;
		
		for (ch = 0; ch < RWW(n).ch; ch++) {
			/* Avoid unnecessary conversion */
			if (!RWW(n).p[ch]) {
				pos += ssize;
				continue;
			}
			buf = sbuf_make_private(sbuf_alloc(bufsize, n));
			wav_read_convert(buf, bufsize, ssize, pad, 
			                 RWW(n).block_align, pos);
			sbuf_queue(RWW(n).p[ch], buf);
			pos += ssize;
		}
		pos += (bufsize-1)*RWW(n).block_align;
		done += blksize;
		filterparam_val_set_pos(pos_param, done);
		bufsize = blksize;
	} while(to_go--);
		
	for (ch = 0; ch < RWW(n).ch; ch++)
		sbuf_queue(RWW(n).p[ch], NULL);

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;
	
	FILTER_RETURN;
}

#ifdef HAVE_AUDIOFILE
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
	sprintf(info, "%d", RWA(n).frameCount); 
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

	RWA(n).frameSize = afGetFrameSize(RWA(n).file, AF_DEFAULT_TRACK, 1);
	sprintf(info, "%d", RWA(n).frameSize);
	filterparam_set_property(fparam,"#framesize", info);

	RWA(n).sampleRate = (int)afGetRate(RWA(n).file, AF_DEFAULT_TRACK);
	sprintf(info, "%d Hz", RWA(n).sampleRate);
	filterparam_set_property(fparam,"#samplerate", info);
	DPRINTF("samplerate : %s\n", info);

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
		
	if ((RWA(n).buffer=(short int*)malloc(GLAME_WBUFSIZE*RWA(n).frameSize))==NULL){
		DPRINTF("Couldn't allocate buffer\n");
		return -1;
	}
	RWA(n).cbuffer=(char *)RWA(n).buffer;
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
	short *b;
	int fcnt, cnt;
	long pos;
	filter_param_t *pos_param;

	/* seek to start of audiofile */
	afSeekFrame(RWA(n).file, AF_DEFAULT_TRACK, 0);
	fcnt = RWA(n).frameCount;

	FILTER_AFTER_INIT;
	pos_param = filterparamdb_get_param(filter_paramdb(n), FILTERPARAM_LABEL_POS);
        filterparam_val_set_pos(pos_param, 0);
	pos = 0;

	while(fcnt){
		FILTER_CHECK_STOP;
		if (!(frames=afReadFrames(RWA(n).file, AF_DEFAULT_TRACK, 
					  RWA(n).buffer,
					  MIN(GLAME_WBUFSIZE, fcnt))))
			break;
		pos += frames;
		filterparam_val_set_pos(pos_param, 0);
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
	filterportdb_foreach_port(filter_portdb(n), port) {
		if (filterport_is_input(port))
			continue;
		filterport_foreach_pipe(port, p_out)
			sbuf_queue(p_out, NULL);
	}

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;

	return 0;
}

void af_read_cleanup(filter_t *n)
{
	free(RWA(n).buffer);
	free(RWA(n).track);
	afCloseFile(RWA(n).file);
	memset(&(RWPRIV(n)->u), 0, sizeof(RWPRIV(n)->u));
}

int af_write_f(filter_t *n)
{
	filter_pipe_t *in;
	filter_port_t *port;
	char *filename;
	int res=-1;
	int eofs,bufsiz,wbpos;
	int i,iat,iass;
	
	RWA(n).channelCount = filterport_nrpipes(
		filterportdb_get_port(filter_portdb(n), PORTNAME_IN));

	filename = filterparam_val_string(
		filterparamdb_get_param(filter_paramdb(n), "filename"));
	if (!filename)
		FILTER_ERROR_RETURN("no filename");

	if (RWA(n).channelCount==0)
		FILTER_ERROR_RETURN("no inputs");
	if (!(RWA(n).track=ALLOCN(RWA(n).channelCount,track_t)))
		FILTER_ERROR_RETURN("no memory");
	
	iass=0;
	filterportdb_foreach_port(filter_portdb(n), port) {
		if (filterport_is_output(port))
			continue;
		filterport_foreach_pipe(port, in) {
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
		} while ((wbpos<bufsiz) && (eofs));
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

#ifdef HAVE_LAME
/* borrowed from lame source */
#define         MAX_U_32_NUM            0xFFFFFFFF

static int
check_aid(const unsigned char *header)
{
    return 0 == strncmp(header, "AiD\1", 4);
}

/*
 * Please check this and don't kill me if there's a bug
 * This is a (nearly?) complete header analysis for a MPEG-1/2/2.5 Layer I, II or III
 * data stream
 */

static int
is_syncword_mp123(const void *const headerptr)
{
    const unsigned char *const p = headerptr;
    static const char abl2[16] =
        { 0, 7, 7, 7, 0, 7, 0, 0, 0, 0, 0, 8, 8, 8, 8, 8 };

    if ((p[0] & 0xFF) != 0xFF)
        return 0;       // first 8 bits must be '1'
    if ((p[1] & 0xE0) != 0xE0)
        return 0;       // next 3 bits are also
    if ((p[1] & 0x18) == 0x08)
        return 0;       // no MPEG-1, -2 or -2.5
    if ((p[1] & 0x06) == 0x00)
        return 0;       // no Layer I, II and III
    if ((p[2] & 0xF0) == 0xF0)
        return 0;       // bad bitrate
    if ((p[2] & 0x0C) == 0x0C)
        return 0;       // no sample frequency with (32,44.1,48)/(1,2,4)    
    if ((p[1] & 0x06) == 0x04) // illegal Layer II bitrate/Channel Mode comb
        if (abl2[p[2] >> 4] & (1 << (p[3] >> 6)))
            return 0;
    return 1;
}

static int
is_syncword_mp3(const void *const headerptr)
{
    const unsigned char *const p = headerptr;

    if ((p[0] & 0xFF) != 0xFF)
        return 0;       // first 8 bits must be '1'
    if ((p[1] & 0xE0) != 0xE0)
        return 0;       // next 3 bits are also
    if ((p[1] & 0x18) == 0x08)
        return 0;       // no MPEG-1, -2 or -2.5
    if ((p[1] & 0x06) != 0x02)
        return 0;       // no Layer III (can be merged with 'next 3 bits are also' test, but don't do this, this decreases readability)
    if ((p[2] & 0xF0) == 0xF0)
        return 0;       // bad bitrate
    if ((p[2] & 0x0C) == 0x0C)
        return 0;       // no sample frequency with (32,44.1,48)/(1,2,4)    
    return 1;
}

int
lame_decode_initfile(FILE * fd, mp3data_struct * mp3data)
{
    //  VBRTAGDATA pTagData;
    // int xing_header,len2,num_frames;
    unsigned char buf[100];
    int     ret;
    int     len, aid_header;
    short int pcm_l[1152], pcm_r[1152];

    memset(mp3data, 0, sizeof(mp3data_struct));
    lame_decode_init();

    len = 4;
    if (fread(&buf, 1, len, fd) != len)
        return -1;      /* failed */
    aid_header = check_aid(buf);
    if (aid_header) {
        if (fread(&buf, 1, 2, fd) != 2)
            return -1;  /* failed */
        aid_header = (unsigned char) buf[0] + 256 * (unsigned char) buf[1];
        fprintf(stderr, "Album ID found.  length=%i \n", aid_header);
        /* skip rest of AID, except for 6 bytes we have already read */
        fseek(fd, aid_header - 6, SEEK_CUR);

        /* read 4 more bytes to set up buffer for MP3 header check */
        len = fread(&buf, 1, 4, fd);
    }


    /* look for valid 4 byte MPEG header  */
    if (len < 4)
        return -1;
    while (!is_syncword_mp123(buf)) {
        int     i;
        for (i = 0; i < len - 1; i++)
            buf[i] = buf[i + 1];
        if (fread(buf + len - 1, 1, 1, fd) != 1)
            return -1;  /* failed */
    }
    // now parse the current buffer looking for MP3 headers.   
    // (as of 11/00: mpglib modified so that for the first frame where 
    // headers are parsed, no data will be decoded.  
    // However, for freeformat, we need to decode an entire frame,
    // so mp3data->bitrate will be 0 until we have decoded the first
    // frame.  Cannot decode first frame here because we are not
    // yet prepared to handle the output.
    ret = lame_decode1_headers(buf, len, pcm_l, pcm_r, mp3data);
    if (-1 == ret)
        return -1;

    /* repeat until we decode a valid mp3 header.  */
    while (!mp3data->header_parsed) {
        len = fread(buf, 1, sizeof(buf), fd);
        if (len != sizeof(buf))
            return -1;
        ret = lame_decode1_headers(buf, len, pcm_l, pcm_r, mp3data);
        if (-1 == ret)
            return -1;
    }

    if (mp3data->bitrate==0) {
        fprintf(stderr,"Input file is freeformat.\n");
    }

    if (mp3data->totalframes > 0) {
        /* mpglib found a Xing VBR header and computed nsamp & totalframes */
    }
    else {
        /* set as unknown.  Later, we will take a guess based on file size
         * ant bitrate */
        mp3data->nsamp = MAX_U_32_NUM;
    }
    /*
       fprintf(stderr,"ret = %i NEED_MORE=%i \n",ret,MP3_NEED_MORE);
       fprintf(stderr,"stereo = %i \n",mp.fr.stereo);
       fprintf(stderr,"samp = %i  \n",freqs[mp.fr.sampling_frequency]);
       fprintf(stderr,"framesize = %i  \n",framesize);
       fprintf(stderr,"bitrate = %i  \n",mp3data->bitrate);
       fprintf(stderr,"num frames = %ui  \n",num_frames);
       fprintf(stderr,"num samp = %ui  \n",mp3data->nsamp);
       fprintf(stderr,"mode     = %i  \n",mp.fr.mode);
     */

    return 0;
}

int lame_decode_fromfile(FILE * fd, short pcm_l[], short pcm_r[], mp3data_struct * mp3data)
{	int     ret = 0, len=0;
	unsigned char buf[4096];

	/* first see if we still have data buffered in the decoder: */
	ret = lame_decode1_headers(buf, len, pcm_l, pcm_r, mp3data);
	if (ret!=0) return ret;


	/* read until we get a valid output frame */
	while (1) {
		len = fread(buf, 1, 4096, fd);
		if (len == 0) {
			/* we are done reading the file, but check for buffered data */
			ret = lame_decode1_headers(buf, len, pcm_l, pcm_r, mp3data);
			if (ret<=0) return -1;  // done with file
			break;
		}
		
		ret = lame_decode1_headers(buf, len, pcm_l, pcm_r, mp3data);
		if (ret == -1) return -1;
		if (ret >0) break;
	}
	return ret;
}

off_t  lame_get_file_size ( const char* const filename )
{
    struct stat       sb;

    if ( 0 == stat ( filename, &sb ) )
        return sb.st_size;
    return (off_t) -1;
}

int lame_read_prepare(filter_t *n, const char *filename)
{
	char buffer[128];
	double flen, totalseconds;
	unsigned long tmp_num_samples;
	filter_param_t	*fparam;
	
	DPRINTF("lame_read_prepare\n");

	if (pthread_mutex_trylock(&lamelock)==EBUSY)
		return -1;

	RWM(n).lgflags = lame_init();
	RWM(n).infile = fopen(filename, "rb");
	if (RWM(n).infile==NULL) {
		pthread_mutex_unlock(&lamelock);
		return -1;
	}

	if (-1 == lame_decode_initfile(RWM(n).infile, &(RWM(n).mp3data))) {
		pthread_mutex_unlock(&lamelock);
		return -1;
	}

	if (lame_set_num_channels(RWM(n).lgflags, RWM(n).mp3data.stereo)==-1) {
		pthread_mutex_unlock(&lamelock);
		return -1;
	}

        lame_set_in_samplerate(RWM(n).lgflags, RWM(n).mp3data.samplerate );
	
	if ((lame_get_num_samples(RWM(n).lgflags) == MAX_U_32_NUM) && (RWM(n).mp3data.bitrate>0)) {
		flen = lame_get_file_size(filename);
		if(flen>=0) {
			totalseconds = (flen * 8.0 / (1000.0 * RWM(n).mp3data.bitrate));
			tmp_num_samples = totalseconds * lame_get_in_samplerate(RWM(n).lgflags);

			lame_set_num_samples(RWM(n).lgflags, tmp_num_samples );
			RWM(n).mp3data.nsamp = tmp_num_samples;
		}
	}

	
	RWM(n).start = fseek(RWM(n).infile,
			     0, 
			     SEEK_CUR);

	DPRINTF("Found mp3 file: channels=%d, freq=%d, offset=%d\n", 
		RWM(n).mp3data.stereo, RWM(n).mp3data.samplerate, 
		RWM(n).start);
	RWM(n).track = ALLOCN(RWM(n).mp3data.stereo, track_t);
	
	fparam = filterparamdb_get_param(filter_paramdb(n), "filename");
	
	sprintf(buffer, "7 -1");
	filterparam_set_property(fparam, "#format", buffer);

	sprintf(buffer,"%d Hz",RWM(n).mp3data.samplerate); 
	filterparam_set_property(fparam, "#samplerate", buffer); 
	
	sprintf(buffer,"16 bit"); 
	filterparam_set_property(fparam, "#quality", buffer); 
	
	sprintf(buffer,"%d",tmp_num_samples); 
	filterparam_set_property(fparam, "#framecount", buffer); 
	
	sprintf(buffer,"%d",RWM(n).mp3data.stereo); 
	filterparam_set_property(fparam, "#channels", buffer);
	
	pthread_mutex_unlock(&lamelock);
	return 0;
}

int lame_read_connect(filter_t *n, filter_pipe_t *p) {
	int i, deleted = 1;
	filter_port_t	*outp;
	filter_pipe_t	*out;
	
	outp = filterportdb_get_port(filter_portdb(n), PORTNAME_OUT);
	filterport_foreach_pipe(outp, out) {
		if (p == out)
			deleted = 0;
	}
	
	if (deleted == 1) {
		for (i=0; i<RWM(n).mp3data.stereo; i++) {
			if (RWM(n).track[i].p == p) {
				RWM(n).track[i].mapped = 0;
				RWM(n).track[i].p = NULL;
				return 0;
			}
		}
	}
	
        for(i=0; i<RWM(n).mp3data.stereo;i++) {
		if (RWM(n).track[i].mapped == 1) {
			if (RWM(n).track[i].p == p) {
				filterpipe_settype_sample(p, RWM(n).mp3data.samplerate,
							     i*M_PI+FILTER_PIPEPOS_LEFT);
				return 0;
			}
		} else if ((RWM(n).track[i].mapped == 0)) {
			RWM(n).track[i].mapped = 1;
			RWM(n).track[i].p = p;
			filterpipe_settype_sample(p, RWM(n).mp3data.samplerate,
						     i*M_PI+FILTER_PIPEPOS_LEFT);
			return 0;
		} 
	}

	return -1;
};

void lame_read_cleanup(filter_t *n) {
	DPRINTF("cleanup\n");
	fclose(RWM(n).infile);
	free(RWM(n).track);
	memset(&(RWPRIV(n)->u), 0, sizeof(RWPRIV(n)->u));
	DPRINTF("cleanup finished\n");
};

int lame_read_f(filter_t *n) {
	filter_param_t *pos_param;
	filter_pipe_t *p_out;
	filter_port_t *port;
	int done, i, j, skip=1, off=0;
	short s[2][1152];
	long pos;

	if (pthread_mutex_trylock(&lamelock)==EBUSY)
		FILTER_ERROR_RETURN("lamelib already in use");
	
	fseek(RWM(n).infile, RWM(n).start, SEEK_SET);
	FILTER_AFTER_INIT;
	pos_param = filterparamdb_get_param(filter_paramdb(n), FILTERPARAM_LABEL_POS);
        filterparam_val_set_pos(pos_param, 0);
	pos = 0;
	do {
		FILTER_CHECK_STOP;
		done = lame_decode_fromfile(RWM(n).infile, s[0], s[1], &(RWM(n).mp3data));
		if(skip==1) {
			done-=528;
			off=528;
		}

		pos += done;

		filterparam_val_set_pos(pos_param, pos);
		if (done > 0) {
			for(i=0; i<RWM(n).mp3data.stereo; i++) {
				RWM(n).track[i].buf = sbuf_make_private(sbuf_alloc(done, n));
				for (j=off; j<done+off; j++)
					sbuf_buf(RWM(n).track[i].buf)[j] = SHORT2SAMPLE(s[i][j]);
				sbuf_queue(RWM(n).track[i].p, RWM(n).track[i].buf);
			}
		}
		if(skip==1) {
			off = 0;
			skip = 0;
		}
	} while (done>0);

	FILTER_BEFORE_STOPCLEANUP;
	FILTER_BEFORE_CLEANUP;
	filterportdb_foreach_port(filter_portdb(n), port) {
		if (filterport_is_input(port))
			continue;
		filterport_foreach_pipe(port, p_out)
			sbuf_queue(p_out, NULL);
	}

	pthread_mutex_unlock(&lamelock);
	return 0;
}

#endif

#endif
