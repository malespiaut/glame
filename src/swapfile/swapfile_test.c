/*
 * swapfile_test.c
 * $Id: swapfile_test.c,v 1.1 2000/01/20 14:54:19 richi Exp $
 *
 * Copyright (C) 1999, 2000 Richard Guenther, Alexander Ehlert
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


#include <stdio.h>
#include <errno.h>
#include "swapfile.h"

#define MEG (1024*1024)
#define FILESIZE (2*MEG)


/* #define BAILOUT return -1 */
#define BAILOUT fprintf(stderr, "raising SIGSEGV for easy debugging\n"); *((char *)0) = 0
#define BUG(str) do { fprintf(stderr, "BUG in " __PRETTY_FUNCTION__ ": " str "\n"); perror("errno"); BAILOUT; } while (0)



int trash_swap()
{
	fileid_t fid = -1, oldfid = -1;
	filecluster_t *fc;
	off_t size, pos, step;
	char *mem;
	int i;

	return 0;
	/* FIXME */

	step = CLUSTER_MINSIZE;
	for (size = CLUSTER_MINSIZE; ; size += step) {
		step += CLUSTER_MINSIZE;
		oldfid = fid;
		if ((fid = file_alloc(size)) == -1)
			BUG("file_alloc");

		/* data should be all zero */
		pos = 0;
		do {
			if (!(fc = filecluster_get(fid, pos)))
				BUG("filecluster_get");
			if (!(mem = filecluster_mmap(fc))) {
				if (errno == ENOMEM)
					break;
				else
					BUG("filecluster_mmap");
			}
			for (i = 0; i < filecluster_size(fc); i++)
				if (mem[i])
					BUG("memory not zeroed");
			filecluster_munmap(fc);
			pos += filecluster_size(fc);
		} while (pos < size);

		/* free file */
		if (oldfid != -1)
			file_unref(oldfid);

		printf("."); fflush(stdout);
	}
	printf("\n");

	file_unref(fid);
	file_unref(oldfid);

	return 0;
}


int test_file_alloc()
{
  fileid_t fid;
  filecluster_t *fc;
  char *mem;
  off_t pos;
  int i;
  int dum[10];

  printf("Allocating file with size=%i\n",FILESIZE);
  if ((fid = file_alloc(FILESIZE)) == -1)
    BUG("file_alloc");

  printf("fid = %i\n",fid);

  for(i=0;i<10;i++){
	  dum[i]=file_alloc(FILESIZE);
	  printf("Try %i fid=%i\n",i,dum[i]);
  }
  /* data should be all zero */
  pos = 0;
  do {
    if (!(fc = filecluster_get(fid, pos)))
      BUG("filecluster_get");
    if (!(mem = filecluster_mmap(fc)))
      BUG("filecluster_mmap");
    for (i = 0; i < filecluster_size(fc); i++)
      if (mem[i])
	BUG("memory not zeroed");
    filecluster_munmap(fc);
    pos += filecluster_size(fc);
  } while (pos < FILESIZE);

  /* file_unref(fid); - for debugging file left in swap */

  for(i=0;i<10;i++) 
	  if (dum[i]) 
		  file_unref(dum[i]);
  return 0;
}

int test_filecluster_rw()
{
  fileid_t fid[4];
  filecluster_t *fc;
  char *mem,*cdum;
  long *dum;
  int pos;
  int i;
  long pat[2];
  char cpat[4]="abcd";
  
  pat[0]=0xaffe0123;
  pat[1]=0xfafb4567;
 
  printf("Testing big files!\n");
  for(i=0;i<2;i++){
	  printf("Allocing file %i...\n",i);
	  if ((fid[i] = file_alloc(FILESIZE*sizeof(long)*(i+1))) == -1) BUG("file_alloc");
  
	  printf("writing data...\n");
          pos=0;
          do {
		  if (!(fc = filecluster_get(fid[i], pos))) BUG("filecluster_get");
	  	  if (!(mem = filecluster_mmap(fc))) BUG("filecluster_mmap");
	  	  dum=(long *)mem;
	  	  while (dum-(long *)mem<filecluster_size(fc)/sizeof(long)) *dum++=pat[i];
	  	  filecluster_munmap(fc);
	  	  pos+=filecluster_size(fc);
	  } while (pos<file_size(fid[i]));
	  printf("finished.\n");
  }

  for(i=0;i<2;i++){
	  printf("Reading data from file %i...\n",i);
	  pos=0;
	  do {
		  if (!(fc = filecluster_get(fid[i], pos))) BUG("filecluster_get");
		  if (!(mem = filecluster_mmap(fc))) BUG("filecluster_mmap");
		  dum=(long *)mem;
		  while (dum-(long *)mem<filecluster_size(fc)/sizeof(long))
			  if (*dum++!=pat[i]) BUG("data differs!");
		  filecluster_munmap(fc);
		  pos+=filecluster_size(fc);
	  } while (pos<file_size(fid[i]));
	  printf("finished.\n");
	  file_unref(fid[i]);
  }
 
  
  printf("Testing small files!\n");
 
  for(i=0;i<4;i++){
	  printf("Writing data to file %i...\n",i);
	  if ((fid[i] = file_alloc(MEG/4*(i+1))) == -1) BUG("file_alloc");
  	  pos=0;
  	  do { 
	  	if (!(fc = filecluster_get(fid[i], pos))) BUG("filecluster_get");
	  	if (!(mem = filecluster_mmap(fc))) BUG("filecluster_mmap");
		cdum=mem;
		while(cdum-mem<filecluster_size(fc)) *cdum++=cpat[i];
		filecluster_munmap(fc);
		pos+=filecluster_size(fc);
	  } while (pos<file_size(fid[i]));
	  printf("finished.\n");
  }

  for(i=0;i<4;i++){
	  pos=0;
	  printf("Reading data from file %i...\n",i);
	  do {
		  if (!(fc = filecluster_get(fid[i], pos))) BUG("filecluster_get");
		  if (!(mem = filecluster_mmap(fc))) BUG("filecluster_mmap");
		  cdum=mem;
		  while(cdum-mem<filecluster_size(fc))
			  if (*cdum++!=cpat[i]) BUG("data differs!");
		  filecluster_munmap(fc);
		  pos+=filecluster_size(fc);
	  } while (pos<file_size(fid[i]));
	  printf("finished.\n");
	  /* Files are not unrefed to make second run harder! */
  }
  return 0;
}


int test_all()
{
  if (test_file_alloc() == -1
      || test_filecluster_rw() == -1)
    return -1;

  return 0;
}

int do_tests()
{
  printf("Entering do_tests\n");
  /* first, clean run */
  if (test_all() == -1)
    return -1;

  printf("first run: test_all done\n");
  
  /* trash swap */
  printf("trying to aggressively fragment swap-space\n");
  if (trash_swap() == -1)
    return -1;
  printf("done fragmenting\n");

  /* second, trashed run */
  if (test_all() == -1)
    return -1;
  
  printf("second run: test_all done\n");
  return 0;
}

int main(int argc, char **argv)
{
  if (argc < 2)
    goto _usage;

  if (swap_open(argv[1], 0) == -1) {
    perror("swap_open");
    return 1;
  }

  if (do_tests() == -1)
    fprintf(stderr, "swapfile tests failed!\n");

  swap_close();

  fprintf(stderr, "all swapfile tests passed.\n");

  return 0;

_usage:
  fprintf(stderr, "Usage: %s swapfile\n", argv[0]);
  return 1;
}


