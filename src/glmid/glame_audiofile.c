/*
 * $Id: glame_audiofile.c,v 1.23 2001/12/17 15:53:40 nold Exp $
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

#define _AF_READ		0
#define _AF_WRITE		1

#define _AF_BUFFER_FRAMES	128
struct _AFfilehandle {
	FILE		*fp;	/* filp */
	AFframecount	cnt;	/* Nr of frames */
	int		ffmt;	/* File format */
	int		ch;	/* Nr of channels in file */
	int		sfmt;	/* Sample format */
	int		width;	/* Sample width */
	int		mode;	/* one of _AF_READ/_AF_WRITE */
	int		err;	/* Most recent error code */	
	union {
		struct {
			size_t	size;	/* Size in bytes of data region */
			long	start;	/* Start of data region */
			long	data;	/* Current pointer to data region */
			int	block_align;	/* Frame alignment */
			int	bps;	/* Bytes per second */
			int	freq;	/* Sample rate */
			void	*buffer;
		} wav;
	} u;
			
};


struct _AFfilesetup {
	int	ffmt;	/* File format */
	int	ch;	/* Number of channels */
	int	sfmt;	/* Sample format */
	int	width;	/* Sample width */
	int	rate;	/* Sample rate */
	int	cmpr;	/* Compression type (not implemented) */
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

	RWW(h).data = ftell(h->fp);
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

static int wav_write_header(AFfilehandle h)
{
	gl_s32 tmp32;
	gl_s16 tmp16;
	
	if (fwrite("RIFF", 4, 1, h->fp) != 1)
		goto err;
	RWW(h).start = ftell(h->fp);
	tmp32 = 0;
	/* Don't know size yet, fixup later. */
	if (fwrite(&tmp32, 4, 1, h->fp) != 1)
		goto err;
	if (fwrite("WAVEfmt ", 8, 1, h->fp) != 1)
		goto err;
	/* chunk size */
	tmp32 = __gl_cpu_to_le32(16);
	if (fwrite(&tmp32, 4, 1, h->fp) != 1)
		goto err;
	/* wav type (only PCM supported) */
	tmp16 = __gl_cpu_to_le16(WAV_FMT_PCM);
	if (fwrite(&tmp16, 2, 1, h->fp) != 1)
		goto err;
	/* number of channels */
	tmp16 = __gl_cpu_to_le16(h->ch);
	if (fwrite(&tmp16, 2, 1, h->fp) != 1)
		goto err;
	/* sample rate */
	tmp32 = __gl_cpu_to_le32(RWW(h).freq);
	if (fwrite(&tmp32, 4, 1, h->fp) != 1)
		goto err;
	/* avg. bytes per second */
	tmp32 = __gl_cpu_to_le32(RWW(h).freq*RWW(h).block_align);
	if (fwrite(&tmp32, 4, 1, h->fp) != 1)
		goto err;
	/* frame alignment */
	tmp16 = __gl_cpu_to_le16(RWW(h).block_align);
	if (fwrite(&tmp16, 2, 1, h->fp) != 1)
		goto err;
	/* sample width in bits */
	tmp16 = __gl_cpu_to_le16(h->width);
	if (fwrite(&tmp16, 2, 1, h->fp) != 1)
		goto err;
	if (fwrite("data", 4, 1, h->fp) != 1)
		goto err;
	RWW(h).data = ftell(h->fp);
	tmp32 = 0;
	/* Don't know size yet, fixup later. */
	if (fwrite(&tmp32, 4, 1, h->fp) != 1)
		goto err;

	return 0;
err:
	return -1;
}	

/* Fixup header information once the file size is known.  Basic philosophy
 * here is: See how far we can get, and don't bother if something fails.
 * The data is already written, and most readers can work on broken header
 * info as well.
 */
static void wav_header_fixup(AFfilehandle h)
{
	long end;
	gl_s32 size;
	
	end = ftell(h->fp);
	if (end > 0x7ffffff) {
		DPRINTF("Warning! File size >= 2GB. Exceeds wav limitation.\n");
		return;
	}
	
	/* data chunk */
	if (!fseek(h->fp, RWW(h).data, SEEK_SET)) {
		size = __gl_cpu_to_le32(end - RWW(h).data - 4);
		fwrite(&size, 4, 1, h->fp);
	}
	
	/* RIFF chunk */
	if (!fseek(h->fp, RWW(h).start, SEEK_SET)) {
		size = __gl_cpu_to_le32(end - RWW(h).start - 4);
		fwrite(&size, 4, 1, h->fp);
	}
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
	
	if (!strcmp(mode, "r"))
		h->mode = _AF_READ;
	else if (!strcmp(mode, "w"))
		h->mode = _AF_WRITE;
	else
		goto err;

	h->fp = fopen(filename, mode);
	if (!h->fp) {
		DPRINTF("%s", strerror(errno));
		goto err;
	}

	/* Wave specific part starts here. Will need to clean up if we
	 * choose to support more file types one day. */
	if (h->mode == _AF_READ) {
		/* Prepare for file read */
		RWW(h).data = 0;
		h->ch = 0;

		if (wav_read_parse(h))
			goto err;

		h->ffmt = AF_FILE_WAVE;
	} else {
		/* Prepare for file write */
		if (!setup)
			goto err;
	
		h->ffmt = setup->ffmt;
		h->ch = setup->ch;
		h->sfmt = setup->sfmt;
		h->width = setup->width;
		RWW(h).freq = setup->rate;
		RWW(h).block_align = h->ch*h->width/8;

		/* XXX: Beware! Assumes wav only. */
		if (wav_write_header(h) == -1)
			goto err;
	}	
	
	if (!(RWW(h).buffer = malloc(_AF_BUFFER_FRAMES * RWW(h).block_align)))
		goto err;
		
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

	if (file->mode == _AF_WRITE)
		wav_header_fixup(file);
	
	if (RWW(file).buffer)
		free(RWW(file).buffer);

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
	in = (void *)RWW(file).buffer;
	if (!in) {
		DPRINTF("Out of memory.\n");
		goto out;
	}
	
	while (frameCount) {
		frames = _AF_BUFFER_FRAMES;
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
	RWW(file).data = ftell(file->fp);

	return total - frameCount;
}

static void from_float(float *in, void *out, int sfmt, int width, int len)
{
	switch (sfmt) {
	case AF_SAMPFMT_UNSIGNED:
		switch (width) {
		case 8: {
			gl_u8 *dst = (gl_u8 *)out;
			while (len--) {
				*dst++ = SAMPLE2UCHAR(*in);
				in++;
			}
			}
			break;
		case 16: {
			gl_u16 *dst = (gl_u16 *)out;
			while (len--) {
				*dst++ = gl_cpu_to_le16(SAMPLE2USHORT(*in));
				in++;
			}
			}
			break;
		}
		break;
	case AF_SAMPFMT_TWOSCOMP:
		switch (width) {
		case 8: {
			gl_s8 *dst = (gl_s8 *)out;
			while (len--) {
				*dst++ = SAMPLE2CHAR(*in);
				in++;
			}
			}
			break;
		case 16: {
			gl_s16 *dst = (gl_s16 *)out;
			while (len--) {
				*dst++ = gl_cpu_to_le16(SAMPLE2SHORT(*in));
				in++;
			}
			}
			break;
		}
		break;
	case AF_SAMPFMT_FLOAT:
		memcpy(out, in, len*sizeof(float));
		break;
	case AF_SAMPFMT_DOUBLE: {
		double *dst = (double *)out;
		while (len--)
			*dst++ = *in++;
		}
		break;
	}
}

int afWriteFrames (AFfilehandle file, int track, const void *buffer, int frameCount)
{
	int frames, total, done;
	
	total = 0;

	if (!handle_ok(file))
		goto err;
	
	if (track != AF_DEFAULT_TRACK)
		return -1;
	
	while (frameCount) {
		frames = _AF_BUFFER_FRAMES;
		if (frameCount < frames)
			frames = frameCount;
		
		from_float((float *)buffer, RWW(file).buffer, file->sfmt,
		           file->width, frames*file->ch);
		done = fwrite(RWW(file).buffer, RWW(file).block_align,
		              frames, file->fp);
		frameCount -= done;
		total += done;
		((float *)buffer) += done*file->ch;

		if (done < frames)
			goto err;
	}

err:
	return total;
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
	AFfilesetup setup;

	setup = malloc(sizeof(*setup));
	return setup;
}

void afFreeFileSetup (AFfilesetup setup)
{
	if (setup)
		free(setup);
}

void afInitFileFormat (AFfilesetup setup, int format)
{
	if (!setup)
		return;

	if (format != AF_FILE_WAVE) {
		DPRINTF("File format %d not supported by wrapper.\n", format);
		return;
	}	
		
	setup->ffmt = format;
}

void afInitChannels (AFfilesetup setup, int track, int nchannels)
{
	if (!setup)
		return;

	if (track != AF_DEFAULT_TRACK)
		return;

	if (nchannels <= 0)
		return;
	
	setup->ch = nchannels;
}

void afInitSampleFormat (AFfilesetup setup, int track, int sampleFormat,
		        int sampleWidth)
{
	if (!setup)
		return;

	if (track != AF_DEFAULT_TRACK)
		return;

	if (sampleWidth <= 0 || sampleWidth & 0x7)
		return;
	
	switch (sampleFormat) {
	case AF_SAMPFMT_TWOSCOMP:
		if (sampleWidth > 16)
			DPRINTF("Not yet supported");
		if (sampleWidth < 16)
			DPRINTF("Warning. Selected format violates standard. "
			        "Try using unsigned.\n");
		break;
	case AF_SAMPFMT_UNSIGNED:
		if (sampleWidth > 16)
			DPRINTF("Not yet supported");
		if (sampleWidth > 8)
			DPRINTF("Warning. Selected format violates standard. "
			        "Try using twoscomp.\n");
		break;
	case AF_SAMPFMT_FLOAT:
		if (sampleWidth != 32)
			DPRINTF("Only 32bit width supported. Adjusting.\n");
		sampleWidth = 32;
		break;
	case AF_SAMPFMT_DOUBLE:
		if (sampleWidth != 64)
			DPRINTF("Only 64bit width supported. Adjusting.\n");
		sampleWidth = 64;
		break;
	default:
		DPRINTF("Unsupported sample format %d.\n", sampleFormat);
	}

	setup->sfmt = sampleFormat;
	setup->width = sampleWidth;
}

void afInitRate (AFfilesetup setup, int track, double rate)
{
	if (!setup)
		return;

	if (track != AF_DEFAULT_TRACK)
		return;

	if (rate <= 0.0)
		return;

	setup->rate = rate;
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
	/* Tough one: audiofile uses a callback scheme for error handling.
	 * Glame currently doesn't make use of it, so no point in implementing
	 * it in the wrapper. [dk]
	 */
	if (track != AF_DEFAULT_TRACK
	    || compression != AF_COMPRESSION_NONE)
		return;
	return;
}

#endif
