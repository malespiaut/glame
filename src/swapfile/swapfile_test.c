/*
 * swapfile_test.c
 * $Id: swapfile_test.c,v 1.14 2000/10/09 16:24:03 richi Exp $
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


/* Fix gcc-isms if we dont have gcc. */
#if !defined HAVE_GCC || defined __cplusplus 
#define __PRETTY_FUNCTION__ __FILE__
#endif


/* Generate nice progress output with the following functions */
static char prefix[80];
static int sequence;
static int quiet = 0;   /* quiet==1 for no output */
static int niceo = 0;    /* niceo==1 for nice output */
static int slow = 0;    /* slow==1 for sleep(1) between messages */
static int raisesigsegv = 1; /* raise a SIGSEGV for debugging? */


/* These have to be macros because of __PRETTY_FUNCTION__ */
#define BUG(str) do { \
	fprintf(stderr, "\nBUG in " __PRETTY_FUNCTION__ ": " str "\n"); \
	perror("errno"); \
	if (raisesigsegv) { \
		fprintf(stderr, "raising SIGSEGV for easy debugging\n"); \
		*((char *)0) = 0; \
	} \
	return -1; \
} while (0)

#define state_start() do { \
	sequence = 0; \
	sprintf(prefix, __PRETTY_FUNCTION__); \
	if (!quiet) { \
		printf("starting %s", prefix); \
		fflush(stdout); \
	} \
} while (0)

static void print_state(const char *templ, ...)
{
	va_list args;

	if (quiet)
		return;

	sequence++;
	if (niceo)
		printf("\r                                                                            \r");
	printf("\r%s: %3i. ", prefix, sequence);
	va_start(args, templ);
	vprintf(templ, args);
	va_end(args);
	if (niceo)
		fflush(stdout);
	else
		printf("\n");

	if (slow)
		sleep(1);
}

static void state_end()
{
	if (quiet)
		return;
	printf("\r%s done.                                                       \n", prefix);
}



/***************************************************************************
 * helpers for data initialisation and checking
 */

/* Random data. */
static char data[4096];

void initialize_data()
{
	int i, *d = (int *)data;

	for (i=0; i<4096/sizeof(int); i++)
		d[i] = rand();
}

int write_random(swfd_t fd, off_t from, off_t size)
{
	return -1;
}

int check_random(swfd_t fd, off_t from, off_t size)
{
	return -1;
}

/* Write to file fd from position from size bytes out of data[].
 * Return 0 if everything went ok, else -1. */
int write_value(swfd_t fd, off_t from, off_t size, char val)
{
	struct sw_stat stat;
	char *mem;
	off_t to = from + size;
	int i;

	if (size == 0)
		return 0;

	do {
		if (sw_lseek(fd, from, SEEK_SET) == -1)
			BUG("Cannot seek");
		if (sw_fstat(fd, &stat) == -1)
			BUG("Cannot stat");
		if (!(mem = (char *)sw_mmap(NULL, PROT_READ|PROT_WRITE,
					    MAP_SHARED, fd)))
			BUG("Cannot mmap");
		for (i=from-stat.cluster_start;
		     i<MIN(stat.cluster_size, to-stat.cluster_start); i++)
			mem[i] = val;
		if (sw_munmap(mem) == -1)
			BUG("Cannot munmap");
		from += (MIN(stat.cluster_size, to-stat.cluster_start)
			 - (from-stat.cluster_start));
	} while (from < to);

	return 0;
}

/* Check, if in file fd from position from size bytes have the
 * value val. Return 0 if this is true, -1 if not, or if any
 * other error occured. */
int check_value(swfd_t fd, off_t from, off_t size, char val)
{
	struct sw_stat stat;
	char *mem;
	off_t to = from + size;
	int i;

	if (size == 0)
		return 0;

	do {
		if (sw_lseek(fd, from, SEEK_SET) == -1)
			BUG("Cannot seek");
		if (sw_fstat(fd, &stat) == -1)
			BUG("Cannot stat");
		if (!(mem = (char *)sw_mmap(NULL, PROT_READ|PROT_WRITE,
					    MAP_SHARED, fd)))
			BUG("Cannot mmap");
		for (i=from-stat.cluster_start;
		     i<MIN(stat.cluster_size, to-stat.cluster_start); i++)
			if (mem[i] != val) {
				sw_munmap(mem);
				return -1;
			}
		if (sw_munmap(mem) == -1)
			BUG("Cannot munmap");
		from += (MIN(stat.cluster_size, to-stat.cluster_start)
			 - (from-stat.cluster_start));
	} while (from < to);

	return 0;
}

/* Check the size of the file fid to be size, return -1 if this
 * is not the case, else 0. */
int check_file_size(swfd_t fd, off_t size)
{
	struct sw_stat stat;

	if (sw_fstat(fd, &stat) == -1)
		BUG("cannot stat");
	if (stat.size != size) {
		print_state("size is %lli should be %lli",
			    (long long)stat.size,
			    (long long)size);
		BUG("file size test failed");
	}

	return 0;
}


/**********************************************************************
 * The actual testing routines.
 */

int test_simple_file()
{
	struct sw_stat stat;
	swfd_t fd;

	state_start();

	/* First simply create the file. Should be of size zero
	 * after this, with expected stat data. */
	print_state("Creating new file");
	if ((fd = sw_open(17, O_RDWR|O_CREAT|O_TRUNC, TXN_NONE)) == -1)
		BUG("Cannot create file");
	if (sw_fstat(fd, &stat) == -1)
		BUG("Cannot stat new file");
	if (stat.name != 17
	    || stat.size != 0
	    || stat.mode != O_RDWR
	    || stat.offset != 0)
		BUG("Wrong stat data");
	if (sw_lseek(fd, 0, SEEK_CUR) != 0)
		BUG("Wrong file position reported from sw_lseek");

	/* Second, truncate the file to larger size and check
	 * for correct perzeroing. */
	print_state("Truncating and checking for zeroed blocks");
	if (sw_ftruncate(fd, 128763) == -1)
		BUG("Cannot truncate");
	if (check_value(fd, 0, 128763, 0) == -1)
		BUG("File not correctly prezeroed");

	/* Third, write some data and read it back. */
	print_state("Writing some data and reading it back");
	if (write_value(fd, 0, 128763, 178) == -1)
		BUG("Cannot write to file");
	if (check_value(fd, 0, 128763, 178) == -1)
		BUG("Cannot read from file");

	/* Fourth, close and unlink file and check if
	 * we can correctly not open it again. */
	print_state("Checking close/unlink/open");
	if (sw_close(fd) == -1)
		BUG("Cannot close file");
	if (sw_unlink(stat.name) == -1)
		BUG("Cannot unlink file");
	if ((fd = sw_open(stat.name, O_RDWR, TXN_NONE)) != -1)
		BUG("Can open unlinked file");

	state_end();
	return 0;
}

int test_sw_open()
{
	return -1;
}

int test_sw_ftruncate()
{
	return -1;
}

int test_sw_sendfile()
{
	return -1;
}




/*********************************************************************
 * Global control.
 */

int test_all()
{
	if (test_simple_file() == -1)
		return -1;
	return 0;
}

int do_tests()
{
	/* first, clean run */
	printf("\nStarting single-threaded run of all tests\n\n");
	if (test_all() == -1)
		return -1;
	printf("\nSingle-threaded run of all tests succesfully completed\n");

	/* FIXME: do multithreaded run */
	return 0;
}

int main(int argc, char **argv)
{
	int res;

	if (argc < 2)
		goto _usage;

 _retry:
	if (swapfile_open(argv[1], 0) == -1) {
		if (errno != ENOENT) {
			perror("Cannot open swap");
			return -1;
		}
		fprintf(stderr, "Cannot open swap - trying to create one...");
		if (swapfile_creat(argv[1], 32*1024*1024) == -1) {
			perror(" failed");
			return 1;
		}
		fprintf(stderr, " ok.\n");
		goto _retry;
	}

	initialize_data();
	res = do_tests();

	swapfile_close();

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
