#ifndef _GLFILEIO_H
#define _GLFILEIO_H

/*
 * glfileio.h
 * $Id: glfileio.h,v 1.3 2001/10/17 09:27:55 richi Exp $
 *
 * Copyright (C) 2001 Alexander Ehlert
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
 */

#include "list.h"


/* define known datatypes */

enum {
	GLFILE_UNKNOWN = -1,
	GLFILE_RAWDATA = 0,
	GLFILE_AIFFC   = 1,
        GLFILE_AIFF    = 2,
        GLFILE_NEXTSND = 3,
        GLFILE_WAVE    = 4,
        GLFILE_BICSF   = 5,
        GLFILE_IRCAM   = AF_FILE_BICSF,
	GLFILE_OGG     = 6,
	GLFILE_MP3     = 7
};

static char glfile_label[8][10] = { "raw", "aiffc", "aiff", "snd", "wave", "bicsf", "ogg", "mp3" };

/* this data type defines a reader or writer plugin */

typedef struct {
	struct glame_list_head list;

} glfileio_plugin_t;


/* used to return queried information */

enum {
	GLFILE_QUERY_POINTER,
	GLFILE_QUERY_INT,
	GLFILE_QUERY_CHAR
};

typedef struct {
	/* data type depending on query */
	int type;
	/* length of returned buffer */
	int length; 
	/* data */
	union {
		int  *p;
		char *c;
		int i;
	} u;
} glfileio_query_t;

/* file context for openend file */

typdef struct {
	glfileio_plugin_t* rwplugin;
	int format;
} glfileio_file_t;

/* register glame readers and writers */
void init_glfileio();

/* open file */
glfileio_file_t *glfile_open(const char*);

/* close file */
void glfile_close(glfileio_file_t*);

/* query file */
glfileio_query_t *glfile_query(glfileio_file_t*);


/* plugin queries */

enum {
	/* return index array of supported formats */
	GLFILE_QUERY_SUPPORTED
};

/* query reader/writer plugin */
glfileio_query_t *glfile_plugin_query(glfileio_plugin_t*);


/* richi's version */

#define GLFILE_VALID_SAMPLERATE 1
#define GLFILE_VALID_NR_CHANNELS 2
#define GLFILE_VALID_NR_SAMPLES 4
#define GLFILE_VALID_FILETYPE 8
typedef struct {
	int valid;
	int samplerate;
	int nr_channels;
	long nr_samples;
	/* etc. */

	int filetype;
	union {
		struct {
		} blaficken_mp3_special_stuff;
		/* etc. */
	};
} glfile_t;

glfile_t *glfile_alloc();
int glfile_open(const char *name, glfile_t *file);
int glfile_creat(const char *name, glfile_t *file);
void glfile_close(glfile_t *file);

int glfile_read(glfile_t *file, int channel, long pos, long cnt, float *data);
int glfile_write(glfile_t *file, int channel, long pos, long cnt, float *data);

char *glfile_query(glfile_t *file, int which);

#endif
