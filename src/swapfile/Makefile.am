INCLUDES = -I@top_srcdir@/src/include $(GLAME_DCFLAGS) -D_REENTRANT -D_GNU_SOURCE

EXTRA_DIST = swfs_cluster.h swfs_cluster.c swfs_file.h swfs_file.c \
	swfs_ctree.h swfs_ctree.c glame-convert.sh \
	pmap.h pmap.c txn.h txn.c

bin_SCRIPTS = glame-convert.sh
noinst_LTLIBRARIES = libswapfile.la
noinst_PROGRAMS = swapfile_mkswap swapfile_fsck

libswapfile_la_SOURCES = \
	swapfile_fs.c swapfile.h

swapfile_fsck_SOURCES = \
	swapfile_fsck.c
swapfile_fsck_LDADD = libswapfile.la
swapfile_mkswap_SOURCES = \
	swapfile_mkswap.c
swapfile_mkswap_LDADD = libswapfile.la
