INCLUDES = -I@top_srcdir@/src/include

noinst_LTLIBRARIES = libhash.la


libhash_la_SOURCES = glame_hash.h glame_hash.c \
	gldb.h gldb.c gldb_worm.h gldb_worm.c \
	gldb_string.h gldb_string.c \
	glsignal.h glsignal.c \
	glsimd.h glsimd.c
libhash_la_LDFLAGS = -export-dynamic

glsimd.lo:
	$(LTCOMPILE) @extra_cflag_unroll@ -c -o $@ @srcdir@/glsimd.c
