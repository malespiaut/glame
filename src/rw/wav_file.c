/*
 * wav_file.c
 * $Id: wav_file.c,v 1.1 2000/01/20 14:54:19 richi Exp $
 *
 * Copyright (C) 2000 Alexander Ehlert
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include "channel.h"
#include "wav_file.h"
#include "swapfile.h"
#include "glame_types.h"


typedef struct {                /* header for WAV-Files */
        char    main_chunk[4];  /* 'RIFF' */
        ulong   length;         /* length of file */
        char    chunk_type[4];  /* 'WAVE' */
        char    sub_chunk[4];   /* 'fmt' */
        ulong   length_chunk;   /* length sub_chunk, always 16 bytes */
        ushort  format;         /* always 1 = PCM-Code */
        ushort  modus;          /* 1 = Mono, 2 = Stereo */
        ulong   sample_fq;      /* Sample Freq */
        ulong   byte_p_sec;     /* Data per sec */
        ushort  byte_p_spl;     /* bytes per sample, 1=8 bit, 2=16 bit (mono)
                                                     2=8 bit, 4=16 bit (stereo) */
        ushort  bit_p_spl;      /* bits per sample, 8, 12, 16 */
        char    data_chunk[4];  /* 'data' */
        ulong   data_length;    /* length of data */
} wave_header;



int wav_read(const char *filename, const char *group)
{
        int fd;
        struct stat sb;
        void * region = NULL;
        wave_header *hdr;
	int channel_length;
	fileid_t file;
	int i,inc;
	short *isample;
	SAMPLE *osample;
	off_t pos;
        char *mem;
	filecluster_t *fc;
	int is_byte;
	int res = -1;

        if ((fd = open(filename,O_RDONLY)) < 0) {
                perror("open");
                return -1;
        }

        if (fstat(fd, &sb)) {
                perror("fstat");
                goto _nomap;
        }

        region = mmap(NULL,sb.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
        if (region == ((caddr_t) -1)) {
                perror("mmap");
                goto _nomap;
        }

        close(fd);

        hdr = (wave_header*)region;
        channel_length=hdr->data_length / hdr->byte_p_spl;
	is_byte=2-(hdr->byte_p_spl/hdr->modus);
	if ( (hdr->modus==2) && (is_byte) ) inc=1;
	else inc=2-is_byte;
	
	for(i=0;i<hdr->modus;i++){
	  isample=(short *)(hdr+1);
	  if (!(is_byte)) isample+=i;
	  file=file_alloc(channel_length*SAMPLE_SIZE);
	  pos=0;
	  do {
	    fc = filecluster_get(file,pos);
	    mem = filecluster_mmap(fc);
	    osample=(SAMPLE *)mem;
	    do {
	      if ( (is_byte) && (hdr->modus==1) )	/* mono audio 8bit */
	      {
		      *osample++=(SAMPLE)((*isample<<8)>>8);	/* get lowbyte first. ENDIAN?? FIXME?! */
			/* FIXME check for cluster boundary... */
		      *osample++=(SAMPLE)(*isample>>8);	/* get highbyte */
	      }
	      else if (is_byte)				/* stereo audio 8bit */
	      {
		      if (i==0) *osample++=(SAMPLE)((*isample<<8)>>8);
		      else *osample++=(SAMPLE)(*isample>>8);
	      }
	      else *osample++=(SAMPLE)*isample;		/* mono/stereo 16bit */
	      isample+=inc;
	    } while (osample-(SAMPLE *)mem<filecluster_size(fc));
	    filecluster_munmap(fc);
	    pos+=filecluster_size(fc);
	  } while (pos<channel_length*SAMPLE_SIZE);
	  add_channel(group,
		      hdr->modus == 1 ? "mono" : (i == 0 ? "left" : "right"),
		      file, i, hdr->byte_p_sec/hdr->byte_p_spl);
	}

	res = 0;
	goto _noinit;

_nomap:
	close(fd);
_noinit:
	if (region)
	  munmap(region,sb.st_size);

	return res;
}


int wav_write(const char *filename, const char *group)
{
  int fd;
  int i=0;
  int done,chan_cnt,len;
  int res=-1;
  int b_off;
  int pos[2];
  filecluster_t *fc[2];
  char *mem[2];
  wave_header *hdr;
  channel_t *chan[2];
  short *buffer;
  SAMPLE *isample[i];
  rw_opt_t opts;
  
  chan_cnt=num_channel(group);
  if ( (chan_cnt<=0) || (chan_cnt>2) )
  {
	  printf("wav_write: chan_cnt=%i\n",chan_cnt);
	  return -1;
  }
  
  if ((fd = open(filename,O_WRONLY)) < 0)
  {
	  perror("open");
	  return -1;
  }

  if (rw_get_opts(group, &opts) == -1)
          goto _noopts;

  if (!(hdr=(wave_header *)malloc(sizeof(wave_header))))
	  goto _nomem;

  strncpy(hdr->main_chunk,"RIFF",4);
  
  chan[0]=get_first_channel(group);
  len=file_size(chan[0]->fid)/SAMPLE_SIZE;	/* length in samples */
  if ((chan[1]=get_next_channel(chan[0])))
	  len+=file_size(chan[1]->fid)/SAMPLE_SIZE;
  hdr->length=sizeof(wave_header)+len*opts.byte_p_smp;
  
  strncpy(hdr->chunk_type,"WAVE",4);
  strncpy(hdr->sub_chunk,"fmt",4);
  hdr->length_chunk=16;
  hdr->format=1;         	  /* always 1 = PCM-Code */
  hdr->modus=opts.mode;	  /* 1 = Mono, 2 = Stereo */
  hdr->sample_fq=opts.freq;      /* Sample Freq */
  hdr->byte_p_sec=opts.freq*opts.mode*opts.byte_p_smp;     /* Data per sec */
  hdr->byte_p_spl=opts.byte_p_smp;     /* bytes per sample, 1=8 bit, 2=16 bit (mono) 2=8 bit, 4=16 bit (stereo) */
  hdr->bit_p_spl=opts.bit_p_smp;       /* bits per sample, 8, 12, 16 */
  strncpy(hdr->data_chunk,"data",4);
  hdr->data_length=len*opts.byte_p_smp;	/* length of data */
						
  if (write(fd,hdr,sizeof(wave_header)) == -1)
    goto _freemem;

  if (!(buffer=(short *)malloc(GLAME_WBUFSIZ*sizeof(short))))
    goto _freemem;
  
  for(i=0;i<chan_cnt;i++)
  {
	  fc[i] = filecluster_get(chan[i]->fid,0);
	  mem[i] = filecluster_mmap(fc[i]);
	  isample[i] = (SAMPLE *)mem[i];
  }
  if (opts.byte_p_smp==2)
  {
	  b_off=0;
	  done=0;
	  pos[0]=pos[1]=0;
	  while (!(done))
	  {
	  	while ( (b_off<GLAME_WBUFSIZ) && (!(done)) )
	  	{
			  for(i=0;i<chan_cnt;i++){
				  *buffer++=(short)*isample[i]++;	/* FIXME more subtle conversion from float to short needed */
				  if(isample[i]-(SAMPLE *)mem[i]>=filecluster_size(fc[i]))
				  {
					  filecluster_munmap(fc[i]);
					  pos[i]+=filecluster_size(fc[i]);
					  if ( (fc[i]=filecluster_next(fc[i]))==NULL ){
						  mem[i] = filecluster_mmap(fc[i]);
						  isample[i]=(SAMPLE *)mem[i];
					  } else done=1;
				  }
			  }
					  
			  b_off+=chan_cnt;
	  	}
		if (!(write(fd,buffer,b_off))) goto _freemem1;
		buffer-=b_off;
		b_off=0;
	
	  }
	  
	  res=1;
  }
  else
  {
	  /* FIXME */
  }
  
_freemem1:
	free(buffer);
_freemem:
	free(hdr);  
_nomem:
_noopts:
	close(fd);
	return res;
}
