#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifndef HAVE_AUDIOFILE
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

AFframecount afSeekFrame (AFfilehandle, int track, AFframecount frameoffset)
{
}

int afReadFrames (AFfilehandle, int track, void *buffer, int frameCount)
{
}

int afWriteFrames (AFfilehandle, int track, const void *buffer, int frameCount)
{
}


AFframecount afGetFrameCount (AFfilehandle file, int track)
{
}

int afGetFileFormat (AFfilehandle, int *version)
{
}

int afGetChannels (AFfilehandle, int track)
{
}

void afGetSampleFormat (AFfilehandle file, int track, int *sampfmt,
		        int *sampwidth)
{
}

float afGetVirtualFrameSize (AFfilehandle, int track, int expand3to4)
{
}

double afGetRate (AFfilehandle, int track)
{
}


AFfilesetup afNewFileSetup (void)
{
	return NULL;
}

void afFreeFileSetup (AFfilesetup)
{
}

void afInitFileFormat (AFfilesetup, int format)
{
}

void afInitChannels (AFfilesetup, int track, int nchannels)
{
}

void afInitSampleFormat (AFfilesetup, int track, int sampleFormat,
		        int sampleWidth)
{
}

void afInitRate (AFfilesetup, int track, double rate)
{
}


#endif
