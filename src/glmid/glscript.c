#include <guile/gh.h>
#include <swapfile.h>
#include "glscript.h"


/* The scriptable swapfile API part.
 */

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

static SCM gls_file_truncate(SCM s_fid, SCM s_size)
{
	fileid_t fid;
	off_t size;

	fid = gh_scm2fileid_t(s_fid);
	size = gh_scm2off_t(s_size);
	if (file_truncate(fid, size) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_file_copy(SCM s_fid, SCM s_pos, SCM s_size)
{
	fileid_t fid, c;
	off_t size, pos;

	fid = gh_scm2fileid_t(s_fid);
	pos = gh_scm2off_t(s_pos);
	size = gh_scm2off_t(s_size);
	if ((c = file_copy(fid, pos, size)) == -1)
		return SCM_BOOL_F;
	return gh_fileid_t2scm(c);
}

static SCM gls_file_op_insert(SCM s_fid, SCM s_pos, SCM s_file)
{
	fileid_t fid, file;
	off_t pos;

	fid = gh_scm2fileid_t(s_fid);
	pos = gh_scm2off_t(s_pos);
        file = gh_scm2fileid_t(s_file);
	if (file_op_insert(fid, pos, file) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_file_op_cut(SCM s_fid, SCM s_pos, SCM s_size)
{
	fileid_t fid;
	off_t size, pos;

	fid = gh_scm2fileid_t(s_fid);
	pos = gh_scm2off_t(s_pos);
	size = gh_scm2off_t(s_size);
	if (file_op_cut(fid, pos, size) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_file_transaction_begin(SCM s_fid)
{
	fileid_t fid;

	fid = gh_scm2fileid_t(s_fid);
	if (file_transaction_begin(fid) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_file_transaction_end(SCM s_fid)
{
	fileid_t fid;

	fid = gh_scm2fileid_t(s_fid);
	if (file_transaction_begin(fid) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_file_transaction_undo(SCM s_fid)
{
	fileid_t fid;

	fid = gh_scm2fileid_t(s_fid);
	if (file_transaction_begin(fid) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}

static SCM gls_file_transaction_redo(SCM s_fid)
{
	fileid_t fid;

	fid = gh_scm2fileid_t(s_fid);
	if (file_transaction_begin(fid) == -1)
		return SCM_BOOL_F;
	return SCM_BOOL_T;
}


/* The scriptable filter API part.
 */

/* FIXME */


int glscript_init()
{
	gh_new_procedure("file_alloc", gls_file_alloc, 1, 0, 0);
	gh_new_procedure("file_unref", gls_file_unref, 1, 0, 0);
	gh_new_procedure("file_size", gls_file_size, 1, 0, 0);
	gh_new_procedure("file_truncate", gls_file_truncate, 2, 0, 0);
	gh_new_procedure("file_copy", gls_file_copy, 3, 0, 0);
	gh_new_procedure("file_op_insert", gls_file_op_insert, 3, 0, 0);
	gh_new_procedure("file_op_cut", gls_file_op_cut, 3, 0, 0);
	gh_new_procedure("file_transaction_begin", gls_file_transaction_begin,
			 1, 0, 0);
	gh_new_procedure("file_transaction_end", gls_file_transaction_end,
			 1, 0, 0);
	gh_new_procedure("file_transaction_undo", gls_file_transaction_undo,
			 1, 0, 0);
	gh_new_procedure("file_transaction_redo", gls_file_transaction_redo,
			 1, 0, 0);

	return 0;
}
