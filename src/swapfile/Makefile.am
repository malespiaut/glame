INCLUDES = -I@srcdir@/../include -D_REENTRANT

noinst_LTLIBRARIES = libswapfile.la
noinst_PROGRAMS = swapfile_test swapfile_info
bin_PROGRAMS = gmkswap


libswapfile_la_SOURCES = swapfile.c swapfile.h swapfileI.h swapfile_hash.h

swapfile_test_SOURCES = swapfile_test.c
swapfile_test_LDADD = libswapfile.la

swapfile_info_SOURCES = swapfile_info.c
swapfile_info_LDADD = libswapfile.la

gmkswap_SOURCES = gmkswap.c


