/*
 * $Id: glame_audiofile.c,v 1.8 2001/11/11 23:38:44 nold Exp $
 *
 * A minimalist wrapper faking an audiofile API to the rest of the world.
 *
 * Copyright (C) 2001 Alexander Ehlert, Richard Guenther, Daniel Kobras
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include "util.h"
#include "glame_audiofile.h"
#include "glame_types.h"
#include "glame_byteorder.h"

/* glame helper functions */
int glame_get_filetype_by_name(char *name) {
	char *suffix, *ausuff;
	int i, type, len;
	int *indices, incnt;
	
	if (!name || !(suffix = strrchr(name, '.')))
		return -1;

	incnt = afQueryLong(AF_QUERYTYPE_FILEFMT, AF_QUERY_ID_COUNT, 0, 0, 0);
	indices = afQueryPointer(AF_QUERYTYPE_FILEFMT, AF_QUERY_IDS, 0, 0, 0);

	suffix++;
	len = strlen(suffix);
	type = -1;
	
	for (i=0; i < incnt; i++) {
		ausuff = (char *)afQueryPointer(AF_QUERYTYPE_FILEFMT,
				                AF_QUERY_LABEL,
					        indices[i], 0, 0);
		if (!strncmp(suffix, ausuff, len)) {
			/* Return immediately if we have an exact match.
			 * Otherwise look on whether there's a better match
			 * hiding. */
			type = indices[i];
			if (strlen(ausuff) == len)
				break;
		}
	}
			
	return type;
}

#ifndef HAVE_AUDIOFILE


/* FIXME: Re-implement wav reader using streams.
 *        Implement wav writer.
 */

struct _AFfilehandle {
	int		fh;	/* File handle (FIXME: better use a filp!) */
	AFframecount	cnt;	/* Nr of frames */
	int		ffmt;	/* File format */
	int		ch;	/* Nr of channels in file */
	int		sfmt;	/* Sample format */
	int		width;	/* Sample width */
	int		err;	/* Most recent error code */	
	union {
		struct {
			caddr_t	map;	/* mmap of audio file */
			size_t	size;	/* Size in bytes of data region */
			char	*start;	/* Start of data region */
			char	*data;	/* Current pointer to data region */
			int	block_align;
			int	bps;
			int	freq;
		} wav;
	} u;
			
};

#define RWW(fh) ((fh)->u.wav)

/* The actual readers and writers.
 */

typedef gl_s32 (*wav_chunk_handler_t)(AFfilehandle h, char *tag, char *pos, 
                                      gl_s32 size);

typedef struct {
	char			*tag;
	wav_chunk_handler_t	handler;
} wav_handlers_t;

#define WAV_FMT_PCM	1

static gl_s32 wav_chunk_ignore(AFfilehandle h, char *tag, char *pos,
                               gl_s32 size)
{
	
	DPRINTF("WAV chunk %s ignored. Skipping %i bytes.\n", tag, size);
	return size;
}

static gl_s32 wav_read_chunk_head(AFfilehandle h, char *tag, char *pos,
                                  gl_s32 size)
{
	if (strncasecmp(pos, "WAVE", 4)) {
		return -1;	/* RIFF but no WAVE */
	}
	
	return 4;
}

static gl_s32 wav_read_chunk_format(AFfilehandle h, char *tag, char *pos,
                                    gl_s32 size)
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
	h->ch = __gl_le16_to_cpup(pos);
	pos += 2;
	RWW(h).freq = __gl_le32_to_cpup(pos);
	pos += 4;
	RWW(h).bps = __gl_le32_to_cpup(pos);
	pos += 4;
	RWW(h).block_align = __gl_le16_to_cpup(pos);
	pos += 2;
	h->width = __gl_le16_to_cpup(pos);
	h->sfmt = h->width == 8 ? AF_SAMPFMT_UNSIGNED : AF_SAMPFMT_TWOSCOMP;
	
	/* Internal limitations */
	if (RWW(h).freq < 2 || !h->ch) {
		DPRINTF("No channels or frequency unreasonably low.\n");
		return -1;
	}
	switch (RWW(h).block_align/h->ch) {
		case 1:
		case 2:
			break;	/* supported */
		default:
			DPRINTF("Unsupported width %d.\n", 
					RWW(h).block_align/h->ch);
			return -1;
	}
	
	return size;
}	

static gl_s32 wav_read_chunk_data(AFfilehandle h, char *tag, char *pos,
                                  gl_s32 size)
{
	if (!h->ch) {
		DPRINTF("No fmt chunk?\n");
		return -1;
	}

	RWW(h).start = RWW(h).data = pos;
	h->cnt = size / RWW(h).block_align;
	if (size % RWW(h).block_align) {
		DPRINTF("WAV data not aligned.\n");
		return -1;
	}
	return size;
}

wav_handlers_t wav_read_handlers[] = {
	{ "RIFF", wav_read_chunk_head },
	{ "fmt ", wav_read_chunk_format },
	{ "data", wav_read_chunk_data },
	{ "fact", wav_chunk_ignore },	/* We only handle uncompressed, ok to
					 * ignore */
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
int wav_read_parse(AFfilehandle h, char *from, char* to)
{
	int i;
	char *tag;
	gl_s32 size;
	wav_chunk_handler_t handler;
	
	h->ch = 0;

	while (from < to) {
		if (to - from < 8) {
			/* Fail gracefully if all required chunks are present */
			DPRINTF("Premature EOF.\n");
			return RWW(h).data ? 0 : -1;
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
			DPRINTF("Illegal size in %s chunk (real: %d, "
			        "adv: %d).\n", tag, to-from, size);
			return -1;
		}
		if ((size = handler(h, tag, from, size)) == -1) {
			DPRINTF("%s handler failed.\n", tag);
			return -1;
		}
		from += size + (size&1);
			
	}
	return 0;
}				

AFfilehandle afOpenFile (const char *filename, const char *mode,
		        AFfilesetup setup)
{
	AFfilehandle h;
	struct stat st;

	h = malloc(sizeof(*h));
	if (!h)
		goto err;

	h->err = AF_ERR_NOT_IMPLEMENTED;
	
	if (strcmp(mode, "r")) {
		goto err;
	}

	h->fh = open(filename, O_RDONLY);
	if (h->fh == -1) {
		DPRINTF("%s", strerror(errno));
		goto err;
	}
	if (fstat(h->fh, &st) == -1) {
		DPRINTF("%s", strerror(errno));
		close(h->fh);
		goto err;
	}

	/* Wave specific part starts here. Will need to clean up if we
	 * choose to support more file types one day. */
	RWW(h).size = st.st_size;
	RWW(h).map = (char *)mmap(NULL, st.st_size, PROT_READ, MAP_PRIVATE,
	                          h->fh, 0);
	
	close(h->fh);

	if (RWW(h).map == MAP_FAILED) {
		DPRINTF("%s", strerror(errno));
		goto err;
	}

	RWW(h).data = NULL;
	h->ch = 0;

	if (wav_read_parse(h, RWW(h).map, RWW(h).map + RWW(h).size)) {
		munmap(RWW(h).map, RWW(h).size);
		goto err;
	}

	h->ffmt = AF_FILE_WAVE;
	
	DPRINTF("Using internal audiofile replacement.\n");
	
	return h;
err:
	if (h) {
		free(h);
	}
	return AF_NULL_FILEHANDLE;
}

int afCloseFile (AFfilehandle file)
{
	if (file == AF_NULL_FILEHANDLE)
		return 0;

	if (file->ffmt == AF_FILE_WAVE)
		munmap(RWW(file).map, RWW(file).size);

	return 0;
	
}

AFframecount afSeekFrame (AFfilehandle file, int track,
                          AFframecount frameoffset)
{
	AFframecount cnt;
	
	if (!file)
		return 0;
	
	if (track != AF_DEFAULT_TRACK)
		return 0;
	
	if (file->ffmt != AF_FILE_WAVE) {
		file->err = AF_ERR_NOT_IMPLEMENTED;
		return 0;
	}
	
	if (cnt >= file->cnt || RWW(file).block_align*cnt > RWW(file).size)
		return 0;

	RWW(file).data = RWW(file).start + RWW(file).block_align*cnt;

	return cnt;
}

int afReadFrames (AFfilehandle file, int track, void *buffer, int frameCount)
{
	unsigned long len;
	
	if (!file)
		return -1;

	if (track != AF_DEFAULT_TRACK)
		return -1;

	len = RWW(file).block_align*frameCount;
	if (RWW(file).start+RWW(file).size < RWW(file).data+len)
		len = RWW(file).start+RWW(file).size-RWW(file).data;

	memcpy(buffer, RWW(file).data, len);
	RWW(file).data += len;

	return len/RWW(file).block_align;
}

int afWriteFrames (AFfilehandle file, int track, const void *buffer, int frameCount)
{
	if (!file)
		goto err;
	
	file->err = AF_ERR_NOT_IMPLEMENTED;
	
err:
	return -1;
}


AFframecount afGetFrameCount (AFfilehandle file, int track)
{
	if (!file)
		return 0;

	return file->cnt;
}

int afGetFileFormat (AFfilehandle file, int *version)
{
	if (!file)
		return AF_FILE_UNKNOWN;

	if (version)
		*version = -1;

	return file->ffmt;
}

int afGetChannels (AFfilehandle file, int track)
{
	if (!file)
		return -1;
	
	if (track != AF_DEFAULT_TRACK)
		return -1;
	
	return file->ch;
}

void afGetSampleFormat (AFfilehandle file, int track, int *sampfmt,
		        int *sampwidth)
{
	if (!file)
		return;

	if (track != AF_DEFAULT_TRACK)
		return;

	*sampfmt = file->sfmt;
	*sampwidth = file->width;
}

float afGetVirtualFrameSize (AFfilehandle file, int track, int expand3to4)
{
	if (!file)
		return 0.0;

	if (track != AF_DEFAULT_TRACK)
		return 0.0;

	return (float) RWW(file).block_align;
}

double afGetRate (AFfilehandle file, int track)
{
	if (!file)
		return 0.0;

	if (track != AF_DEFAULT_TRACK)
		return 0.0;

	return RWW(file).freq;
}


AFfilesetup afNewFileSetup (void)
{
	return NULL;
}

void afFreeFileSetup (AFfilesetup setup)
{
}

void afInitFileFormat (AFfilesetup setup, int format)
{
}

void afInitChannels (AFfilesetup setup, int track, int nchannels)
{
}

void afInitSampleFormat (AFfilesetup setup, int track, int sampleFormat,
		        int sampleWidth)
{
}

void afInitRate (AFfilesetup setup, int track, double rate)
{
}


long afQueryLong (int querytype, int arg1, int arg2, int arg3, int arg4)
{
	return -1;
}

void *afQueryPointer (int querytype, int arg1, int arg2, int arg3, int arg4)
{
	return NULL;
}


#endif
