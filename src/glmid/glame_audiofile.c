/*
 * $Id: glame_audiofile.c,v 1.16 2001/12/16 20:54:09 nold Exp $
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
#include "glsimd.h"
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


/* FIXME: Implement wav writer.
 */

struct _AFfilehandle {
	FILE		*fp;	/* filp */
	AFframecount	cnt;	/* Nr of frames */
	int		ffmt;	/* File format */
	int		ch;	/* Nr of channels in file */
	int		sfmt;	/* Sample format */
	int		width;	/* Sample width */
	int		err;	/* Most recent error code */	
	union {
		struct {
			size_t	size;	/* Size in bytes of data region */
			fpos_t	start;	/* Start of data region */
			fpos_t	data;	/* Current pointer to data region */
			int	block_align;
			int	bps;
			int	freq;
		} wav;
	} u;
			
};

#define RWW(fh) ((fh)->u.wav)

/* The actual readers and writers.
 */

typedef gl_s32 (*wav_chunk_handler_t)(AFfilehandle h, char *tag, gl_s32 size);

typedef struct {
	char			*tag;
	wav_chunk_handler_t	handler;
} wav_handlers_t;

#define WAV_FMT_PCM	1

static gl_s32 wav_chunk_ignore(AFfilehandle h, char *tag, gl_s32 size)
{
	
	DPRINTF("WAV chunk %s ignored. Skipping %i bytes.\n", tag, size);
	fseek(h->fp, size, SEEK_CUR);
	return size;
}

static gl_s32 wav_read_chunk_head(AFfilehandle h, char *tag, gl_s32 size)
{
	char pos[4];

	if (fread(pos, 4, 1, h->fp) != 1)
		return -1;
	if (strncasecmp(pos, "WAVE", 4)) {
		return -1;	/* RIFF but no WAVE */
	}
	
	return 4;
}

static gl_s32 wav_read_chunk_format(AFfilehandle h, char *tag, gl_s32 size)
{
	char tmp[16], *pos;
	
	if (size < 16) {
		DPRINTF("Illegal chunk size.\n");
		return -1;
	}
	
	if (fread(tmp, 16, 1, h->fp) != 1) {
		DPRINTF("Premature EOF in %s chunk.\n", tag);
		return -1;
	}

	pos = tmp;
	
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

	if (size > 16) {
		DPRINTF("%s chunk longer than expected (%d). Skipping.\n",
				tag, size);
		fseek(h->fp, size-16, SEEK_CUR);
	}

	DPRINTF("wav format freq %i, balign %i, ch %i\n", RWW(h).freq, RWW(h).block_align, h->ch);
	
	return size;
}	

static gl_s32 wav_read_chunk_data(AFfilehandle h, char *tag, gl_s32 size)
{
	if (!h->ch) {
		DPRINTF("No fmt chunk?\n");
		return -1;
	}

	fgetpos(h->fp, &(RWW(h).data));
	RWW(h).start = RWW(h).data;
	h->cnt = size / RWW(h).block_align;
	if (size % RWW(h).block_align) {
		DPRINTF("WAV data not aligned.\n");
		return -1;
	}

	fseek(h->fp, size, SEEK_CUR);
	
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
int wav_read_parse(AFfilehandle h)
{
	int i;
	char tag[4], len[4];
	gl_s32 size;
	wav_chunk_handler_t handler;
	
	h->ch = 0;

	while (!feof(h->fp)) {
		if (fread(tag, 4, 1, h->fp) != 1 ||
		    fread(len, 4, 1, h->fp) != 1) {
			/* Fail gracefully if all required chunks are present */
			return RWW(h).data ? 0 : -1;
		}
		for (i=0; (handler=wav_read_handlers[i].handler); i++)
			if (!strncasecmp(tag, wav_read_handlers[i].tag, 4)) 
				break;

		if (!handler) 
			return -1;
		
		size = __gl_le32_to_cpup(len);
		if (size < 0) {
			DPRINTF("Illegal size %d in %s chunk.\n", size, tag);
			return -1;
		}
		DPRINTF("Trying %s handler, size %d.\n", tag, size);
		if ((size = handler(h, tag, size)) == -1) {
			DPRINTF("%s handler failed.\n", tag);
			return -1;
		}
		if (size&1)
			fseek(h->fp, 1, SEEK_CUR);
			
	}
	return 0;
}				

static int handle_ok(AFfilehandle h)
{
	return (h != AF_NULL_FILEHANDLE && h->fp != NULL);
}

AFfilehandle afOpenFile (const char *filename, const char *mode,
		        AFfilesetup setup)
{
	AFfilehandle h;

	h = malloc(sizeof(*h));
	if (!h)
		goto err;

	h->err = AF_ERR_NOT_IMPLEMENTED;
	
	if (strcmp(mode, "r") /* && strcmp(mode, "w") */) {
		goto err;
	}

	h->fp = fopen(filename, mode);
	if (!h->fp) {
		DPRINTF("%s", strerror(errno));
		goto err;
	}

	/* Wave specific part starts here. Will need to clean up if we
	 * choose to support more file types one day. */
	RWW(h).data = 0;
	h->ch = 0;

	if (wav_read_parse(h))
		goto err;

	h->ffmt = AF_FILE_WAVE;
	
	DPRINTF("Using internal audiofile replacement.\n");
	
	return h;
err:
	if (h) {
		if (h->fp)
			fclose (h->fp);
		free(h);
	}
	return AF_NULL_FILEHANDLE;
}

int afCloseFile (AFfilehandle file)
{
	if (!handle_ok(file))
		return 0;

	if (file->fp)
		fclose(file->fp);

	return 0;
	
}

AFframecount afSeekFrame (AFfilehandle file, int track,
                          AFframecount frameoffset)
{
	if (!handle_ok(file))
		return 0;
	
	if (track != AF_DEFAULT_TRACK)
		return 0;
	
	if (file->ffmt != AF_FILE_WAVE) {
		file->err = AF_ERR_NOT_IMPLEMENTED;
		return 0;
	}
	
	if (frameoffset >= file->cnt)
		return 0;

	RWW(file).data = RWW(file).start + RWW(file).block_align*frameoffset;

	return frameoffset;
}


/* Convert input buffer to float.
 * 
 * in - input buffer, out - output buffer,
 * ifmt - input format in audiofile notation,
 * width - sample width of input buffer in bits,
 * ch - number of channels in input buffer,
 * skip - number of bytes to skip after 'ch' samples,
 * frames - number of 'ch*width + skip' sized frames.
 */
static void to_float(void *in, float *out, int ifmt, int width, int ch,
                     int skip, int frames)
{
	switch (ifmt) {
	case AF_SAMPFMT_TWOSCOMP:
		switch (width) {
		case 8: {
			int i, j;
			gl_s8 *src = (gl_s8 *)in;
			for (j=0; j < frames; j++, src++) {
				for (i=0; i < ch; i++)
					*out++ = CHAR2SAMPLE(*src);
				src += skip;
			}
			break;
		}
		case 16: {
			int i, j;
			gl_s16 *src = (gl_s16 *)in;
			for (j=0; j < frames; j++) {
				for (i=0; i < ch; i++, src++)
					*out++ = SHORT2SAMPLE((gl_s16)
						(__gl_le16_to_cpup(src)));
				((char *)src) += skip;
			}
			break;
		}
		default:
			PANIC("Width not supported.");
		}
		break;
	case AF_SAMPFMT_UNSIGNED:
		switch (width) {
		case 8: {
			int i, j;
			gl_u8 *src = (gl_u8 *)in;
			for (j=0; j < frames; j++) {
				for (i=0; i < ch; i++, src++)
					*out++ = UCHAR2SAMPLE(*src);
				src += skip;
			}
			break;
		}
		case 16: {
			int i, j;
			gl_u16 *src = (gl_u16 *)in;
			for (j=0; j < frames; j++) {
				for (i=0; i < ch; i++, src++)
					*out++ = USHORT2SAMPLE((gl_u16)
						(__gl_le16_to_cpup(src)));
				((char *)src) += skip;
			}
			break;
		}
		default:
			PANIC("Width not supported.\n");
		}
		break;
	case AF_SAMPFMT_FLOAT: {
		int i, j;
		float *src = (float *)in;
		if (width != 32)
			PANIC("Width not supported.\n");

		for (j=0; j < frames; j++) {
			for (i=0; i < ch; i++)
				*out++ = *src++;
			((char *)src) += skip;
		}
		}
		break;
	case AF_SAMPFMT_DOUBLE: {
		int i, j;
		double *src = (double *)in;

		if (width != 64)
			PANIC("Width not supported.\n");
		
		for (j=0; j < frames; j++) {
			for (i=0; i < ch; i++)
				*out++ = (float) *src++;
			((char *)src) += skip;
		}
		}
		break;
	}
}


int afReadFrames (AFfilehandle file, int track, void *buffer, int frameCount)
{
	int frames, total;
	float *in = NULL;
	
	if (!handle_ok(file))
		return -1;

	if (track != AF_DEFAULT_TRACK)
		return -1;

	if (fseek(file->fp, RWW(file).data, SEEK_SET) == -1) {
		DPRINTF("Seek failed: %s.\n", strerror(errno));
		return -1;
	}
	
	total = frameCount;
	in = malloc(in, 128*RWW(file).block_align);
	if (!in) {
		DPRINTF("Out of memory.\n");
		goto out;
	}
	
	while (frameCount) {
		frames = 128;
		if (frameCount < frames)
			frames = frameCount;
		
		frames = fread(in, RWW(file).block_align, frames, file->fp);
		frameCount -= frames;
		if (!frames) {
			if (feof(file->fp))
				DPRINTF("Premature EOF.\n");
			else
				DPRINTF("fread: %s.\n", strerror(errno));
			
			goto out;
		}
		
		to_float(in, buffer, file->sfmt, file->width, file->ch,
	                 RWW(file).block_align-file->ch*file->width/8,
			 frames);
		((float *)buffer) += frames*file->ch;
	}
	
out:
	fgetpos(file->fp, &(RWW(file).data));
	if (in)
		free(in);

	return total - frameCount;
}

int afWriteFrames (AFfilehandle file, int track, const void *buffer, int frameCount)
{
	if (!handle_ok(file))
		goto err;
	
	file->err = AF_ERR_NOT_IMPLEMENTED;
	
err:
	return -1;
}


AFframecount afGetFrameCount (AFfilehandle file, int track)
{
	if (!handle_ok(file))
		return 0;

	return file->cnt;
}

int afGetFileFormat (AFfilehandle file, int *version)
{
	if (!handle_ok(file))
		return AF_FILE_UNKNOWN;

	if (version)
		*version = -1;

	return file->ffmt;
}

int afGetChannels (AFfilehandle file, int track)
{
	if (!handle_ok(file))
		return -1;
	
	if (track != AF_DEFAULT_TRACK)
		return -1;
	
	return file->ch;
}

void afGetSampleFormat (AFfilehandle file, int track, int *sampfmt,
		        int *sampwidth)
{
	if (!handle_ok(file))
		return;

	if (track != AF_DEFAULT_TRACK)
		return;

	/* FIXME */
	*sampfmt = /* AF_SAMPFMT_FLOAT; */ file->sfmt;
	*sampwidth = /* 32; */ file->width;
}

float afGetVirtualFrameSize (AFfilehandle file, int track, int expand3to4)
{
	if (!handle_ok(file))
		return 0.0;

	if (track != AF_DEFAULT_TRACK)
		return 0.0;

	/* FIXME */
	return (float) (sizeof(float) * file->ch);
}

double afGetRate (AFfilehandle file, int track)
{
	if (!handle_ok(file))
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
	switch (querytype) {
	case AF_QUERYTYPE_FILEFMT:
		switch (arg1) {
		case AF_QUERY_ID_COUNT:
			return 1;
			break;
		case AF_QUERY_SAMPLE_FORMATS:
			switch(arg2) {
			case AF_QUERY_VALUE_COUNT:
				return 0;
				break;
			default:
				return -1;
			}
			break;
		case AF_QUERY_COMPRESSION_TYPES:
			switch(arg2) {
			case AF_QUERY_VALUE_COUNT:
				return 0;
				break;
			default:
				return -1;
			}
			break;
		default:
			return -1;
		}
		break;
	default:
		return -1;
	}
}

void *afQueryPointer (int querytype, int arg1, int arg2, int arg3, int arg4)
{
	static int glame_audiofile_ids[] = { AF_FILE_WAVE };
	static char *glame_audiofile_labels[] = { [AF_FILE_WAVE] = "wave" };
	switch (querytype) {
	case AF_QUERYTYPE_FILEFMT:
		switch (arg1) {
		case AF_QUERY_IDS:
			return glame_audiofile_ids;
			break;
		case AF_QUERY_LABEL:
			return glame_audiofile_labels[arg2];
			break;
		case AF_QUERY_SAMPLE_FORMATS:
			switch(arg2) {
			default:
				return NULL;
			}
			break;
		default:
			return NULL;
		}
		break;
	default:
		return NULL;
	}
}


int afSetVirtualSampleFormat(AFfilehandle file, int track,
			     int sampleFormat, int sampleWidth) 
{
	if (track != AF_DEFAULT_TRACK
	    || sampleFormat != AF_SAMPFMT_FLOAT
	    || sampleWidth != 32)
		return -1;
	return 0; 
}

int afSetVirtualPCMMapping(AFfilehandle file, int track, 
			   double slope, double intercept,
			   double minClip, double maxClip) 
{
	if (track != AF_DEFAULT_TRACK
	    /* || slope != whats that? */
	    || intercept != 0.0
	    || minClip != -1.0
	    || maxClip != 1.0)
		return -1;
	return 0;
}

void afInitCompression(AFfilesetup setup, int track, int compression)
{
	/* FIXME: error handling!? */
	if (track != AF_DEFAULT_TRACK
	    || compression != AF_COMPRESSION_NONE)
		return;
	return;
}

#endif
