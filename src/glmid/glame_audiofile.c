#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifndef HAVE_AUDIOFILE
#include <stdlib.h>
#include "glame_audiofile.h"


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


#endif
