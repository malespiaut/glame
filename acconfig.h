/* define if you have OSS audio support */
#undef HAVE_OSS

/* define if OSS headers are in linux/soundcard.h */
#undef HAVE_OSS_LINUX

/* define if OSS headers are in sys/soundcard.h */
#undef HAVE_OSS_SYS

/* define if OSS headers are in machine/soundcard.h */
#undef HAVE_OSS_MACHINE

/* define, if you have esd.h and libesd */
#undef HAVE_ESD

/* define if SGI/IRIX audio support is available */
#undef HAVE_SGIAUDIO

/* define if ALSA audio is available */
#undef HAVE_ALSA

/* define if you have sys/asound.h rather than alsa/asound.h */
#undef ALSA_H_IN_SYS

/* define if you have libaudiofile */
#undef HAVE_AUDIOFILE

/* define if you have libsndfile */
#undef HAVE_SNDFILE

/* define if you have fftw of SAMPLE precision */
#undef HAVE_FFTW

/* define if you have guile */
#undef HAVE_GUILE

/* define if you have ladspa */
#undef HAVE_LADSPA

/* define if we have gcc */
#undef HAVE_GCC

/* define which cpu you have */
#undef CPU_X86
#undef CPU_MIPS
#undef CPU_PPC
#undef CPU_ALPHA

/* define if your mips cpu understands ll and sc opcodes */
#undef HAVE_MIPS_LL_SC

/* define which os you use */
#undef OS_LINUX
#undef OS_BSD
#undef OS_IRIX

/* define if you use pthreads (always defined for GLAME) */
#undef USE_PTHREADS

/* define if you have pthread_sigmask(3) available */
#undef HAVE_PTHREAD_SIGMASK

/* define if your libc supports the madvise system call */
#undef HAVE_MADVISE

/* define to include additional debugging code */
#undef DEBUG

/* define to disable default debugging code */
#undef NDEBUG

/* define to include swapfile debugging code */
#undef SWDEBUG

/* define if <byteswap.h> is present */
#undef HAVE_BYTESWAP_H

/* define to the type you want for SAMPLE (float or double) */
#define SAMPLE float

/* define if you dont have sqrtf */
#undef NEED_SQRTF

/* define, if the SAMPLE type is std (float) */
#undef SAMPLE_FLOAT

/* define, if you have libglade */
#undef HAVE_LIBGLADE

/* define, if you have/want libstroke */
#undef HAVE_LIBSTROKE

#undef ENABLE_NLS
#undef HAVE_CATGETS
#undef HAVE_GETTEXT
#undef HAVE_LC_MESSAGES
#undef HAVE_STPCPY
