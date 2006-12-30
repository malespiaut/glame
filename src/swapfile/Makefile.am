INCLUDES = -I@top_srcdir@/src/include -D_GNU_SOURCE

EXTRA_DIST = swfs_cluster.h swfs_cluster.c swfs_file.h swfs_file.c \
	swfs_ctree.h swfs_ctree.c

noinst_LTLIBRARIES = libswapfile.la
noinst_PROGRAMS = swapfile_mkswap swapfile_fsck

libswapfile_la_SOURCES = \
	swapfile_fs.c swapfile.h
libswapfile_la_LDFLAGS = -export-dynamic

swapfile_fsck_SOURCES = \
	swapfile_fsck.c
swapfile_fsck_LDADD = libswapfile.la $(PTHREAD_LIBS)
swapfile_mkswap_SOURCES = \
	swapfile_mkswap.c
swapfile_mkswap_LDADD = libswapfile.la $(PTHREAD_LIBS)
