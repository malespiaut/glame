#include <guile/gh.h>
#include <swapfile.h>
#include "glscript.h"


#define gh_scm2fileid_t gh_scm2long
#define gh_fileid_t2scm gh_long2scm
#define gh_scm2off_t gh_scm2long
#define gh_off_t2scm gh_long2scm 

static SCM gls_file_alloc(SCM s_size)
{
	fileid_t fid;
	off_t size;

	size = gh_scm2off_t(s_size);
	if ((fid = file_alloc(size)) == -1)
		return SCM_BOOL_F;
	return gh_fileid_t2scm(fid);
}

static SCM gls_file_unref(SCM s_fid)
{
	fileid_t fid;

	fid = gh_scm2fileid_t(s_fid);
	file_unref(fid);
	return SCM_UNSPECIFIED;
}

static SCM gls_file_size(SCM s_fid)
{
	fileid_t fid;
	off_t size;

	fid = gh_scm2fileid_t(s_fid);
	if ((size = file_size(fid)) == -1)
		return SCM_BOOL_F;
	return gh_off_t2scm(size);
}

#if 0
static SCM gls_file_truncate(SCM fid)
{
}

static SCM gls_file_copy(SCM fid)
{
}

static SCM gls_file_op_insert(SCM fid, SCM pos, SCM file)
{
}

static SCM gls_file_op_cut(SCM fid, SCM pos, SCM size)
{
}

static SCM gls_file_transaction_begin(SCM fid)
{
}

static SCM gls_file_transaction_end(SCM fid)
{
}

static SCM gls_file_transaction_undo(SCM fid)
{
}

static SCM gls_file_transaction_redo(SCM fid)
{
}
#endif


int glscript_init()
{
	gh_new_procedure("file_alloc", gls_file_alloc, 1, 0, 0);
	gh_new_procedure("file_unref", gls_file_unref, 1, 0, 0);
	gh_new_procedure("file_size", gls_file_size, 1, 0, 0);
	return 0;
}
