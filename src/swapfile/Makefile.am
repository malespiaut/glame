INCLUDES = -I@top_srcdir@/src/include $(GLAME_DCFLAGS) -D_REENTRANT

EXTRA_DIST = swfs_cluster.h swfs_cluster.c swfs_file.h swfs_file.c \
	swfs_ctree.h swfs_ctree.c

noinst_LTLIBRARIES = libswapfile.la
noinst_PROGRAMS = swapfile_test
#noinst_PROGRAMS = swapfile_info
bin_PROGRAMS = gmkswap


libswapfile_la_SOURCES = \
		swapfile_fs.c swapfile.h \
		pmap.h pmap.c \
		txn.h txn.c

swapfile_test_SOURCES = swapfile_test.c
swapfile_test_LDADD = libswapfile.la $(GLAME_DLIBS)

#swapfile_info_SOURCES = swapfile_info.c
#swapfile_info_LDADD = libswapfile.la

gmkswap_SOURCES = gmkswap.c
gmkswap_LDADD = libswapfile.la

