/*
 * swapfile_test.c
 * $Id: swapfile_test.c,v 1.4 2000/01/28 13:00:31 richi Exp $
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
#include <unistd.h>
#include <errno.h>
#include "swapfile.h"

#define MEG (1024*1024)
#define FILESIZE (2*MEG)


/* #define BAILOUT return -1 */
#define BAILOUT fprintf(stderr, "raising SIGSEGV for easy debugging\n"); *((char *)0) = 0
#define BUG(str) do { fprintf(stderr, "\nBUG in " __PRETTY_FUNCTION__ ": " str "\n"); perror("errno"); BAILOUT; } while (0)


/* generate nice progress output with the following functions */
static char prefix[80];
#define state_start() \
do { \
	sprintf(prefix, __PRETTY_FUNCTION__); \
	printf("starting %s", prefix); \
	fflush(stdout); \
} while (0)
#define print_state(str, args...) \
do { \
        printf("\r                                                        "); \
        printf("\r%s: ", prefix); \
	printf(str, ## args); \
	fflush(stdout); \
        sleep(1); \
} while (0)
#define state_end() \
do { \
	printf("\r%s done.                                      \n", prefix); \
} while (0)



/* helpers for "data initialisation" */
int do_zero_file(fileid_t fid)
{
	filecluster_t *fc;
	off_t size;
	char *mem;

	fc = filecluster_get(fid, 0);
	while (fc) {
		if (!(mem = filecluster_mmap(fc)))
			BUG("filecluster_mmap");
		size = filecluster_size(fc);
		while (size--)
			*(mem++) = 0;
		filecluster_munmap(fc);
		fc = filecluster_next(fc);
	}

	return 0;
}

int check_zero_file(fileid_t fid)
{
	filecluster_t *fc;
	off_t size;
	char *mem;

	fc = filecluster_get(fid, 0);
	while (fc) {
		if (!(mem = filecluster_mmap(fc)))
			BUG("filecluster_mmap");
		size = filecluster_size(fc);
		while (size--)
			if (*(mem++) != 0) {
				filecluster_munmap(fc);
				return -1;
			}
		filecluster_munmap(fc);
		fc = filecluster_next(fc);
	}

	return 0;
}

int do_ffff_file(fileid_t fid)
{
	filecluster_t *fc;
	off_t size;
	char *mem;

	fc = filecluster_get(fid, 0);
	while (fc) {
		if (!(mem = filecluster_mmap(fc)))
			BUG("filecluster_mmap");
		size = filecluster_size(fc);
		while (size--)
			*(mem++) = -1;
		filecluster_munmap(fc);
		fc = filecluster_next(fc);
	}

	return 0;
}

int check_ffff_file(fileid_t fid)
{
	filecluster_t *fc;
	off_t size;
	char *mem;

	fc = filecluster_get(fid, 0);
	while (fc) {
		if (!(mem = filecluster_mmap(fc)))
			BUG("filecluster_mmap");
		size = filecluster_size(fc);
		while (size--)
			if (*(mem++) != -1) {
				filecluster_munmap(fc);
				return -1;
			}
		filecluster_munmap(fc);
		fc = filecluster_next(fc);
	}

	return 0;
}

int check_file_size(fileid_t fid, off_t size)
{
	off_t s;

	print_state("Checking file size");
	if ((s = file_size(fid)) == -1)
		return -1;
	if (s != size) {
		print_state("size is %i should be %i", (int)s, (int)size);
		BUG("file size test failed");
	}

	return 0;
}



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


/* test_file_alloc will check the correct operation of
 * - file_alloc, especially correct pre-zeroing
 * - file_size
 * - file_truncate, especially correct pre-zeroing
 * - file_unref
 */
int test_file_alloc()
{
	/* the following constants are initial size and truncate size
	 * (relative, added) for the file */
	static int sizes[] = { 1024*1024, CLUSTER_MINSIZE, -CLUSTER_MINSIZE, 0,
			       1024*1024, 1, 0, -1,
			       1024*1024, 32721, -17, -3781 };
	int nr_sizes = sizeof(sizes)/(4*sizeof(int));
	fileid_t fid;
	off_t size, t1size, t2size, t3size;
	int i;


	state_start();

	for (i=0; i<nr_sizes; i++) {
		size = sizes[4*i];
		t1size = size + sizes[4*i+1];
		t2size = size + sizes[4*i+2];
		t3size = size + sizes[4*i+3];

		print_state("Allocating file with size %i", (int)size);
		if ((fid = file_alloc(size)) == -1)
			BUG("file_alloc");
		check_file_size(fid, size);
		print_state("Testing file for correct pre-zeroing");
		if (check_zero_file(fid) == -1)
			BUG("prezeroing test of file failed");

		print_state("Truncating file to size %i", (int)t1size);
		if (file_truncate(fid, t1size) == -1)
			BUG("file_truncate");
		check_file_size(fid, t1size);
		print_state("Testing file for correct pre-zeroing");
		if (check_zero_file(fid) == -1)
			BUG("prezeroing test of file failed");

		print_state("Truncating file to size %i", (int)t2size);
		if (file_truncate(fid, t2size) == -1)
			BUG("file_truncate");
		check_file_size(fid, t2size);
		print_state("Testing file for correct pre-zeroing");
		if (check_zero_file(fid) == -1)
			BUG("prezeroing test of file failed");

		print_state("Truncating file to size %i", (int)t3size);
		if (file_truncate(fid, t3size) == -1)
			BUG("file_truncate");
		check_file_size(fid, t3size);
		print_state("Testing file for correct pre-zeroing");
		if (check_zero_file(fid) == -1)
			BUG("prezeroing test of file failed");

		print_state("cheking file_unref");
		file_unref(fid);
		if (file_size(fid) != -1)
			BUG("file_unref");
	}

	state_end();
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
  
  state_start();
  pat[0]=0xaffe0123;
  pat[1]=0xfafb4567;
 
  print_state("Testing big files");
  for(i=0;i<2;i++){
	  print_state("Allocing file %i...",i);
	  if ((fid[i] = file_alloc(FILESIZE*sizeof(long)*(i+1))) == -1)
		  BUG("file_alloc");
  
	  print_state("writing data...");
          pos=0;
          do {
		  if (!(fc = filecluster_get(fid[i], pos)))
			  BUG("filecluster_get");
	  	  if (!(mem = filecluster_mmap(fc)))
			  BUG("filecluster_mmap");
	  	  dum=(long *)mem;
	  	  while (dum-(long *)mem<filecluster_size(fc)/sizeof(long))
			  *dum++=pat[i];
	  	  filecluster_munmap(fc);
	  	  pos+=filecluster_size(fc);
	  } while (pos<file_size(fid[i]));
	  print_state("finished.");
  }

  for(i=0;i<2;i++){
	  print_state("Reading data from file %i...",i);
	  pos=0;
	  do {
		  if (!(fc = filecluster_get(fid[i], pos)))
			  BUG("filecluster_get");
		  if (!(mem = filecluster_mmap(fc)))
			  BUG("filecluster_mmap");
		  dum=(long *)mem;
		  while (dum-(long *)mem<filecluster_size(fc)/sizeof(long))
			  if (*dum++!=pat[i]) BUG("data differs!");
		  filecluster_munmap(fc);
		  pos+=filecluster_size(fc);
	  } while (pos<file_size(fid[i]));
	  print_state("finished.");
	  file_unref(fid[i]);
  }
 
  
  print_state("Testing small files!");
 
  for(i=0;i<4;i++){
	  print_state("Writing data to file %i...",i);
	  if ((fid[i] = file_alloc(MEG/4*(i+1))) == -1)
		  BUG("file_alloc");
  	  pos=0;
  	  do { 
	  	if (!(fc = filecluster_get(fid[i], pos)))
			BUG("filecluster_get");
	  	if (!(mem = filecluster_mmap(fc)))
			BUG("filecluster_mmap");
		cdum=mem;
		while (cdum-mem<filecluster_size(fc))
			*cdum++=cpat[i];
		filecluster_munmap(fc);
		pos+=filecluster_size(fc);
	  } while (pos<file_size(fid[i]));
	  print_state("finished.");
  }

  for(i=0;i<4;i++){
	  pos=0;
	  print_state("Reading data from file %i...",i);
	  do {
		  if (!(fc = filecluster_get(fid[i], pos)))
			  BUG("filecluster_get");
		  if (!(mem = filecluster_mmap(fc)))
			  BUG("filecluster_mmap");
		  cdum=mem;
		  while(cdum-mem<filecluster_size(fc))
			  if (*cdum++!=cpat[i])
				  BUG("data differs!");
		  filecluster_munmap(fc);
		  pos+=filecluster_size(fc);
	  } while (pos<file_size(fid[i]));
	  print_state("finished.");
	  /* Files are not unrefed to make second run harder! */
  }
  state_end();
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
	/* first, clean run */
	printf("\nStarting first run of all tests\n\n");
	if (test_all() == -1)
		return -1;
	printf("\nFirst run of all tests succesfully completed\n");
	
	/* trash swap */
	printf("\nTrying to aggressively fragment swap-space\n");
	if (trash_swap() == -1)
		return -1;
	printf("Done fragmenting\n");

	/* second, trashed run */
	printf("\nStarting second run of all tests\n\n");
	if (test_all() == -1)
		return -1;
	printf("\nSecond run of all tests succesfully completed\n");

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
		fprintf(stderr, "\nOne or more swapfile-tests failed!\nAborted.\n");

	swap_close();

	fprintf(stderr, "\nAll swapfile-tests passed.\n");

	return 0;

_usage:
	fprintf(stderr, "Usage: %s swapfile\n", argv[0]);
	return 1;
}


