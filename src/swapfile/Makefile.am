INCLUDES = -I@top_srcdir@/src/include $(GLAME_DCFLAGS) -D_REENTRANT -D_GNU_SOURCE

EXTRA_DIST = swfs_cluster.h swfs_cluster.c swfs_file.h swfs_file.c \
	swfs_ctree.h swfs_ctree.c convert.sh \
	pmap.h pmap.c txn.h txn.c swapfile_test.c swapfile_info.c gmkswap.c

noinst_LTLIBRARIES = libswapfile.la
noinst_SCRIPTS = convert.sh

libswapfile_la_SOURCES = \
		swapfile_fs.c swapfile.h
