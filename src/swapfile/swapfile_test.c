/*
 * swapfile_test.c
 * $Id: swapfile_test.c,v 1.10 2000/04/25 08:56:17 richi Exp $
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
 *
 * TODO:
 * - add more complex transaction patterns to test_file_transaction
 * - fix trash_swap (maybe for the ease of use install a trashing
 *   hook in swapfile.c? uh...)
 * - add a background thread which does random file allocation
 *   and deallocation to test thread safety
 * - think about the numbers - do we test all common cases?
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include "swapfile.h"


/* what to do at a failure? dump core or just be nice and tell about the bug? */
#define BAILOUT return -1
/* #define BAILOUT fprintf(stderr, "raising SIGSEGV for easy debugging\n"); *((char *)0) = 0 */
#define BUG(str) do { fprintf(stderr, "\nBUG in " __PRETTY_FUNCTION__ ": " str "\n"); perror("errno"); BAILOUT; } while (0)

/* define SLOW to sleep(1) if you want to be able to follow all msgs */
#define SLOW
/* define NICE for nice output :) */
#define NICE

/* generate nice progress output with the following functions */
static char prefix[80];
static int sequence;

#ifndef HAVE_GCC
#define __PRETTY_FUNCTION__	__FILE__
#endif

#define state_start() \
do { \
        sequence = 0; \
	sprintf(prefix, __PRETTY_FUNCTION__); \
	printf("starting %s", prefix); \
	fflush(stdout); \
} while (0)
#ifdef NICE
inline void print_state(const char *templ, ...)
{
	va_list args;
	sequence++;
	printf("\r                                                                            ");
	printf("\r%s: %3i. ", prefix, sequence);
	va_start(args, templ);
	vprintf(templ, args);
	va_end(args);
	fflush(stdout);
	SLOW;
}
#else
inline void print_state(const char *templ, ...)
{
	va_list args;
	sequence++;
	printf("%s: %3i. ", prefix, sequence);
	va_start(args, templ);
	vprintf(templ, args);
	va_end(args);
	printf("\n");
	SLOW;
}
#endif	
#define state_end() \
do { \
	printf("\r%s done.                                                       \n", prefix); \
} while (0)



/* helpers for "data initialisation" */
int do_write_file(fileid_t fid, soff_t from, soff_t size, char val)
{
	filecluster_t *fc;
	char *mem;
	soff_t to;

	if (size == 0)
		return 0;
	
	if (!(fc = filecluster_get(fid, from)))
		return -1;
	to = from + size;
	do {
		if (!(mem = filecluster_mmap(fc)))
			return -1;
		for (; from <= filecluster_end(fc) && from < to; from++)
			mem[from - filecluster_start(fc)] = val;
		filecluster_munmap(fc);
		fc = filecluster_next(fc);
	} while (fc && filecluster_start(fc) < to);

	return 0;
}

int check_val_file(fileid_t fid, soff_t from, soff_t size, int val)
{
	filecluster_t *fc;
	char *mem;
	soff_t to;

	if (size == 0)
		return 0;

	to = from + size;
	if (!(fc = filecluster_get(fid, from)))
		return -1;
	do {
		if (!(mem = filecluster_mmap(fc)))
			return -1;
		for (; from <= filecluster_end(fc) && from < to; from++)
			if (mem[from - filecluster_start(fc)] != val) {
				filecluster_munmap(fc);
				return -1;
			}
		filecluster_munmap(fc);
		fc = filecluster_next(fc);
	} while (fc && filecluster_start(fc) < to);

	return 0;
}

int check_file_size(fileid_t fid, soff_t size)
{
	soff_t s;

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
	soff_t size, pos, step;
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
 * - file_truncate, especially correct pre-zeroing
 * - file_size
 * - file_unref
 */
int test_file_alloc()
{
	/* the following constants are initial sizes and truncate sizes
	 * for the file, put the number of truncate sizes after the initial
         * size and an initial size of -1 at the end. */
	static int sizes[] = {
		0, CLUSTER_MINSIZE, 5*CLUSTER_MINSIZE, 2*CLUSTER_MINSIZE, -1,
		CLUSTER_MINSIZE, 413, CLUSTER_MINSIZE+7123, 0, -1,
		CLUSTER_MINSIZE, CLUSTER_MAXSIZE+CLUSTER_MINSIZE, CLUSTER_MAXSIZE-CLUSTER_MINSIZE, -1,
		-1
	};
	fileid_t fid;
	soff_t size, tsize;
	int *sp;


	state_start();

	sp = sizes;
	while (*sp != -1) {
		size = *(sp++);

		print_state("Allocating file with size %i", (int)size);
		if ((fid = file_alloc(size)) == -1)
			BUG("file_alloc");
		check_file_size(fid, size);
		print_state("Testing initial file for correct pre-zeroing");
		if (check_val_file(fid, 0, file_size(fid), 0) == -1)
			BUG("prezeroing test of initial file failed");

		while (*sp != -1) {
			tsize = *(sp++);
			print_state("Truncating file to size %i", (int)tsize);
			if (file_truncate(fid, tsize) == -1)
				BUG("file_truncate");
			check_file_size(fid, tsize);
			print_state("Testing file for correct pre-zeroing");
			if (check_val_file(fid, 0, file_size(fid), 0) == -1)
				BUG("prezeroing test of file failed");
		}

		print_state("cheking file_unref");
		file_unref(fid);
		if (file_size(fid) != -1)
			BUG("file_unref");

		sp++;
	}

	state_end();
	return 0;
}

/* test_filecluster_rw will check
 * - filecluster_get
 * - filecluster_mmap/filecluster_munmap
 * and of course consistency of data written to multiple
 * files
 */
int test_filecluster_rw()
{
	/* {file size, {from, to, value}, -1}, -1
	 * non-overlapping!!! */
	static int sizes[] = {
		CLUSTER_MINSIZE, 0, 1236, 1, 1237, CLUSTER_MINSIZE-1, 2, -1,
		1024*1024, 0, 512*1024, 100, 512*1024+1, 1024*1024-1, 101, -1,
		CLUSTER_MINSIZE, 0, 1236, 3, 1237, CLUSTER_MINSIZE-1, 4, -1,
		1024*1024, 0, 512*1024, 102, 512*1024+1, 1024*1024-1, 103, -1,
		CLUSTER_MINSIZE, 0, 1236, 5, 1237, CLUSTER_MINSIZE-1, 6, -1,
		1024*1024, 0, 512*1024, 104, 512*1024+1, 1024*1024-1, 105, -1,
		CLUSTER_MINSIZE, 0, 1236, 7, 1237, CLUSTER_MINSIZE-1, 8, -1,
		1024*1024, 0, 512*1024, 106, 512*1024+1, 1024*1024-1, 107, -1,
		CLUSTER_MINSIZE, 0, 1236, 9, 1237, CLUSTER_MINSIZE-1, 10, -1,
		1024*1024, 0, 512*1024, 108, 512*1024+1, 1024*1024-1, 109, -1,
		CLUSTER_MINSIZE, 0, 1236, 11, 1237, CLUSTER_MINSIZE-1, 12, -1,
		1024*1024, 0, 512*1024, 110, 512*1024+1, 1024*1024-1, 111, -1,
		-1
	};
	int size, from, to;
	char val;
	fileid_t fid[32];
	int *sp, nr;

	state_start();

	/* first round - allocate the files and write the data. */
	sp = sizes;
	nr = 0;
	while (*sp != -1) {
		size = *(sp++);

		print_state("Allocating file %i with size %i", nr, size);
		if ((fid[nr] = file_alloc(size)) == -1)
			BUG("file_alloc");

		while (*sp != -1) {
			from = *(sp++);
			to = *(sp++);
			val = (char)*(sp++);

			print_state("Writing to file %i, [%i, %i] = %i", nr, from, to, val);
			if (do_write_file(fid[nr], from, to-from+1, val) == -1)
				BUG("error in writing to file");
		}
		sp++;
		nr++;
	}

	/* second round - read back the data and kill the files */
	sp = sizes;
	nr = 0;
	while (*sp != -1) {
		size = *(sp++);

		while (*sp != -1) {
			from = *(sp++);
			to = *(sp++);
			val = (char)*(sp++);

			print_state("Checking file %i contents, [%i, %i] == %i", nr, from, to, val);
			if (check_val_file(fid[nr], from, to-from+1, val) == -1)
				BUG("inconsistent data in file");
		}
		sp++;

		print_state("Unref'ing file %i (was size %i)", nr, (int)size);
		file_unref(fid[nr]);
		nr++;
	}

	state_end();
	return 0;
}

/* test_file_transaction will test
 * - file_copy
 * - file_op_insert
 * - file_op_cut
 * - file_transaction_begin/end
 * - file_transaction_undo/redo
 */
int test_file_transaction()
{
	/* { file1: size, val, cut: from, size, insert: at,
	 *   file2: size, val, cut: from, size, insert: at, } -1 */
	int sizes[] = {
		123877, 18, 273, 32478, 38239,
		92773, 37, 12367, 1237, 14913,
		1024*1024, 33, 128*1024, 1024, 128,
		1024*1024, 99, 512*1024, 1024, 128*1024,
		1487623, 9, 0, 398724, 2378,
		998381, 76, 990000, 8381, 990000,
		-1
	};
	fileid_t f1, f1c, f2, f2c;
	int size1, val1, cutfrom1, cutsize1, insertpos1;
	int size2, val2, cutfrom2, cutsize2, insertpos2;
	int *sp;

	state_start();

	sp = sizes;
	while (*sp != -1) {
		size1 = *(sp++); val1 = *(sp++);
		cutfrom1 = *(sp++); cutsize1 = *(sp++);
		insertpos1 = *(sp++);
		size2 = *(sp++); val2 = *(sp++);
		cutfrom2 = *(sp++); cutsize2 = *(sp++);
		insertpos2 = *(sp++);

		print_state("Allocating two files");
		if ((f1 = file_alloc(size1)) == -1
		    || (f2 = file_alloc(size2)) == -1)
			BUG("file_alloc()");

		print_state("Writing signatures to both files");
		if (do_write_file(f1, 0, size1, val1) == -1
		    || do_write_file(f2, 0, size2, val2) == -1)
			BUG("do_write_file()");

		print_state("Copying part of file one");
		if ((f1c = file_copy(f1, cutfrom1, cutsize1)) == -1)
			BUG("file_copy()");
		check_file_size(f1c, cutsize1);
		print_state("Checking integrity of copy");
		if (check_val_file(f1c, 0, cutsize1, val1) == -1)
			BUG("file integrity compromised");

		print_state("Starting transaction on file one");
		if (file_transaction_begin(f1) == -1)
			BUG("file_transaction_begin");
		print_state("Removing copied part of file one");
		if (file_op_cut(f1, cutfrom1, cutsize1) == -1)
			BUG("file_op_cut");
		check_file_size(f1, size1-cutsize1);
		print_state("Checking integrity of file one");
		if (check_val_file(f1, 0, size1-cutsize1, val1) == -1)
			BUG("file integrity compromised");


		print_state("Copying part of file two");
		if ((f2c = file_copy(f2, cutfrom2, cutsize2)) == -1)
			BUG("file_copy()");
		check_file_size(f2c, cutsize2);
		print_state("Checking integrity of copy");
		if (check_val_file(f2c, 0, cutsize2, val2) == -1)
			BUG("file integrity compromised");

		print_state("Starting transaction on file two");
		if (file_transaction_begin(f2) == -1)
			BUG("file_transaction_begin");
		print_state("Removing copied part of file two");
		if (file_op_cut(f2, cutfrom2, cutsize2) == -1)
			BUG("file_op_cut");
		check_file_size(f2, size2-cutsize2);
		print_state("Checking integrity of file two");
		if (check_val_file(f2, 0, size2-cutsize2, val2) == -1)
			BUG("file integrity compromised");

		print_state("Inserting part of file one into file two");
		if (file_op_insert(f2, insertpos2, f1c) == -1)
			BUG("file_op_insert()");
		check_file_size(f2, size2-cutsize2+cutsize1);
		print_state("Checking integrity of file two");
		if (check_val_file(f2, 0, insertpos2, val2) == -1
		    || check_val_file(f2, insertpos2, cutsize1, val1) == -1
		    || check_val_file(f2, insertpos2+cutsize1, size2-cutsize2-insertpos2, val2) == -1)
			BUG("file integrity compromised");

		print_state("Ending transaction on file two");
		if (file_transaction_end(f2) == -1)
			BUG("file_transaction_end()");
		print_state("Ending transaction on file one");
		if (file_transaction_end(f1) == -1)
			BUG("file_transaction_end()");

		print_state("Undoing transaction of file two");
		if (file_transaction_undo(f2) == -1)
			BUG("file_transaction_undo()");
		check_file_size(f2, size2);
		check_file_size(f1, size1-cutsize1);
		check_file_size(f1c, cutsize1);
		check_file_size(f2c, cutsize2);
		print_state("Checking integrity of file one");
		if (check_val_file(f1, 0, size1-cutsize1, val1) == -1)
			BUG("file integrity compromised");
		print_state("Checking integrity of file two");
		if (check_val_file(f2, 0, size2, val2) == -1)
			BUG("file integrity compromised");
		print_state("Checking integrity of copy of file one");
		if (check_val_file(f1c, 0, cutsize1, val1) == -1)
			BUG("file integrity compromised");
		print_state("Checking integrity of copy of file two");
		if (check_val_file(f2c, 0, cutsize2, val2) == -1)
			BUG("file integrity compromised");

		print_state("Undoing transaction of file one");
		if (file_transaction_undo(f1) == -1)
			BUG("file_transaction_undo()");
		check_file_size(f2, size2);
		check_file_size(f1, size1);
		check_file_size(f1c, cutsize1);
		check_file_size(f2c, cutsize2);
		print_state("Checking integrity of file one");
		if (check_val_file(f1, 0, size1, val1) == -1)
			BUG("file integrity compromised");
		print_state("Checking integrity of file two");
		if (check_val_file(f2, 0, size2, val2) == -1)
			BUG("file integrity compromised");
		print_state("Checking integrity of copy of file one");
		if (check_val_file(f1c, 0, cutsize1, val1) == -1)
			BUG("file integrity compromised");
		print_state("Checking integrity of copy of file two");
		if (check_val_file(f2c, 0, cutsize2, val2) == -1)
			BUG("file integrity compromised");


		print_state("Redoing transaction of file two");
		if (file_transaction_redo(f2) == -1)
			BUG("file_transaction_undo()");
		if (check_file_size(f2, size2-cutsize2+cutsize1) == -1
		    || check_file_size(f1, size1) == -1
		    || check_file_size(f2c, cutsize2) == -1)
			return -1;
		print_state("Checking integrity of file one");
		if (check_val_file(f1, 0, size1, val1) == -1)
			BUG("file integrity compromised");
		print_state("Checking integrity of file two");
		if (check_val_file(f2, 0, insertpos2, val2) == -1
		    || check_val_file(f2, insertpos2, cutsize1, val1) == -1
		    || check_val_file(f2, insertpos2+cutsize1, size2-cutsize2-insertpos2, val2) == -1)
			BUG("file integrity compromised");
		print_state("Checking integrity of copy of file two");
		if (check_val_file(f2c, 0, cutsize2, val2) == -1)
			BUG("file integrity compromised");


		print_state("Unref'ing file one");
		file_unref(f1);
		print_state("Unref'ing file two");
		file_unref(f2);
		print_state("Unref'ing copy of file two");
		file_unref(f2c);
	}

	state_end();
	return 0;
}

int test_all()
{
  if (test_file_alloc() == -1
      || test_filecluster_rw() == -1
      || test_file_transaction() == -1)
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
	int res;

	if (argc < 2)
		goto _usage;

	if (swap_open(argv[1], 0) == -1) {
		perror("swap_open");
		return 1;
	}

	res = do_tests();

	swap_close();

	if (res == -1) {
		fprintf(stderr, "\nOne or more swapfile-tests failed!\nAborted.\n");
		return 1;
	} else {
		fprintf(stderr, "\nAll swapfile-tests passed.\n");
	}

	return 0;

_usage:
	fprintf(stderr, "Usage: %s swapfile\n", argv[0]);
	return 1;
}


