/*
 * $Id: glame_audiofile.c,v 1.7 2001/11/11 17:51:23 nold Exp $
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
#include <string.h>
#include <util.h>
#include "glame_audiofile.h"

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


/* FIXME: implement glame used part of libaudiofile API with
 *        just wav file support for read/write.
 */


AFfilehandle afOpenFile (const char *filename, const char *mode,
		        AFfilesetup setup)
{
	return NULL;
}

int afCloseFile (AFfilehandle file)
{
}

AFframecount afSeekFrame (AFfilehandle file, int track, AFframecount frameoffset)
{
}

int afReadFrames (AFfilehandle file, int track, void *buffer, int frameCount)
{
}

int afWriteFrames (AFfilehandle file, int track, const void *buffer, int frameCount)
{
}


AFframecount afGetFrameCount (AFfilehandle file, int track)
{
}

int afGetFileFormat (AFfilehandle file, int *version)
{
}

int afGetChannels (AFfilehandle file, int track)
{
}

void afGetSampleFormat (AFfilehandle file, int track, int *sampfmt,
		        int *sampwidth)
{
}

float afGetVirtualFrameSize (AFfilehandle file, int track, int expand3to4)
{
}

double afGetRate (AFfilehandle file, int track)
{
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
