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
	
	if (strlen(name)==0)
		return -1;

	suffix = strrchr(name, '.');
	suffix++;
	DPRINTF("Got suffix %s for file %s\n", suffix, name);
	
	DPRINTF("Query audiofile library\n");
	incnt = afQueryLong(AF_QUERYTYPE_FILEFMT, AF_QUERY_ID_COUNT,0 ,0 ,0);

	DPRINTF("audiofile supports %d formats\n", incnt);
	indices = afQueryPointer(AF_QUERYTYPE_FILEFMT, AF_QUERY_IDS, 0 ,0, 0);

	type = -1;
	for(i=0;i<incnt;i++) {
		/* pointer for query_label MUST NOT be freed */
		ausuff = (char*)afQueryPointer(AF_QUERYTYPE_FILEFMT, AF_QUERY_LABEL, indices[i] ,0 ,0);
		DPRINTF("Format %d, suffix %s, res %d\n", indices[i], ausuff);
		if (strcmp(suffix, ausuff)==0) {
			DPRINTF("match found for type %d!\n",indices[i]);
			type = indices[i];
			break;
		};
	};	
	
	/* try a sloppy match */
	if (type==-1) {
		len = strlen(suffix);
		DPRINTF("Sloppy match for %d chars\n", len);
		for(i=0;i<incnt;i++) {
			ausuff = (char*)afQueryPointer(AF_QUERYTYPE_FILEFMT, AF_QUERY_LABEL, indices[i] ,0 ,0);
			if(strncmp(suffix, ausuff, len)==0) {
				type = indices[i];
				break;
			};
		};
	};
			
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
