/*
 * glscript_gpsm.c
 *
 * Copyright (C) 2001 Richard Guenther
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
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <guile/gh.h>
#include "gpsm.h"
#include "glscript.h"


/* SMOB for gpsm_item_t.
 */

long gpsmitem_smob_tag;
#define scm2gpsmitem(s) (gpsm_item_t *)scm2pointer(s, gpsmitem_smob_tag)
#define gpsmitem2scm(p) pointer2scm((void *)p, gpsmitem_smob_tag)
#define scminvalidategpsmitem(s) scminvalidatepointer(s, gpsmitem_smob_tag)
#define gpsmitem_p(s) (SCM_NIMP(s) && SCM_CAR(s) == gpsmitem_smob_tag)



/*
 * The scriptable gpsm API part.
 */

/*
 * Types and access.
 */

static SCM gls_gpsm_is_item(SCM s_item)
{
	if (gpsmitem_p(s_item))
		return SCM_BOOL_T;
	return SCM_BOOL_F;
}

static SCM gls_gpsm_item_parent(SCM s_item)
{
	gpsm_item_t *item;
	SCM_ASSERT(gpsmitem_p(s_item), s_item,
		   SCM_ARG1, "gpsm-item-parent");
	item = scm2gpsmitem(s_item);
	return gpsmitem2scm(gpsm_item_parent(item));
}

static SCM gls_gpsm_item_label(SCM s_item)
{
	gpsm_item_t *item;
	SCM_ASSERT(gpsmitem_p(s_item), s_item,
		   SCM_ARG1, "gpsm-item-label");
	item = scm2gpsmitem(s_item);
	return gh_str02scm(gpsm_item_label(item));
}

static SCM gls_gpsm_item_set_label(SCM s_item, SCM s_label)
{
	gpsm_item_t *item;
	char *label;
	int label_len;
	SCM_ASSERT(gpsmitem_p(s_item), s_item,
		   SCM_ARG1, "gpsm-item-set-label!");
	SCM_ASSERT(gh_string_p(s_label), s_label,
		   SCM_ARG1, "gpsm-item-set-label!");
	item = scm2gpsmitem(s_item);
	label = gh_scm2newstr(s_label, &label_len);
	gpsm_item_set_label(item, label);
	free(label);
	return SCM_UNSPECIFIED;
}

static SCM gls_gpsm_item_hposition(SCM s_item)
{
	gpsm_item_t *item;
	SCM_ASSERT(gpsmitem_p(s_item), s_item,
		   SCM_ARG1, "gpsm-item-hposition");
	item = scm2gpsmitem(s_item);
	return gh_long2scm(gpsm_item_hposition(item));
}

static SCM gls_gpsm_item_vposition(SCM s_item)
{
	gpsm_item_t *item;
	SCM_ASSERT(gpsmitem_p(s_item), s_item,
		   SCM_ARG1, "gpsm-item-vposition");
	item = scm2gpsmitem(s_item);
	return gh_long2scm(gpsm_item_vposition(item));
}

static SCM gls_gpsm_item_hsize(SCM s_item)
{
	gpsm_item_t *item;
	SCM_ASSERT(gpsmitem_p(s_item), s_item,
		   SCM_ARG1, "gpsm-item-hsize");
	item = scm2gpsmitem(s_item);
	return gh_long2scm(gpsm_item_hsize(item));
}

static SCM gls_gpsm_item_vsize(SCM s_item)
{
	gpsm_item_t *item;
	SCM_ASSERT(gpsmitem_p(s_item), s_item,
		   SCM_ARG1, "gpsm-item-vsize");
	item = scm2gpsmitem(s_item);
	return gh_long2scm(gpsm_item_vsize(item));
}


static SCM gls_gpsm_is_grp(SCM s_item)
{
	if (gpsmitem_p(s_item)
	    && GPSM_ITEM_IS_GRP(scm2gpsmitem(s_item)))
		return SCM_BOOL_T;
	return SCM_BOOL_F;
}

static SCM gls_gpsm_grp_items(SCM s_item)
{
	gpsm_item_t *item, *it;
	SCM s_items = SCM_LIST0;
	SCM_ASSERT(gpsmitem_p(s_item), s_item,
		   SCM_ARG1, "gpsm-grp_items");
	item = scm2gpsmitem(s_item);
	if (GPSM_ITEM_IS_GRP(item)) {
		gpsm_grp_foreach_item(item, it)
			s_items = gh_cons(gpsmitem2scm(it), s_items);
	}
	return s_items;
}


static SCM gls_gpsm_is_swfile(SCM s_item)
{
	if (gpsmitem_p(s_item)
	    && GPSM_ITEM_IS_SWFILE(scm2gpsmitem(s_item)))
		return SCM_BOOL_T;
	return SCM_BOOL_F;
}

static SCM gls_gpsm_swfile_filename(SCM s_item)
{
	gpsm_item_t *item;
	SCM_ASSERT(gpsmitem_p(s_item)
		   && (item = scm2gpsmitem(s_item),
		       GPSM_ITEM_IS_SWFILE(item)), s_item,
		   SCM_ARG1, "gpsm-swfile-filename");
	return gh_long2scm(gpsm_swfile_filename(item));
}

static SCM gls_gpsm_swfile_samplerate(SCM s_item)
{
	gpsm_item_t *item;
	SCM_ASSERT(gpsmitem_p(s_item)
		   && (item = scm2gpsmitem(s_item),
		       GPSM_ITEM_IS_SWFILE(item)), s_item,
		   SCM_ARG1, "gpsm-swfile-samplerate");
	return gh_long2scm(gpsm_swfile_samplerate(item));
}

static SCM gls_gpsm_swfile_set_samplerate(SCM s_item, SCM s_rate)
{
	gpsm_item_t *item;
	SCM_ASSERT(gpsmitem_p(s_item)
		   && (item = scm2gpsmitem(s_item),
		       GPSM_ITEM_IS_SWFILE(item)), s_item,
		   SCM_ARG1, "gpsm-swfile-set-samplerate!");
	SCM_ASSERT(gh_exact_p(s_rate), s_rate,
		   SCM_ARG2, "gpsm-swfile-set-samplerate!");
	gpsm_swfile_set_samplerate((gpsm_swfile_t *)item,
				   gh_scm2long(s_rate));
	return SCM_UNSPECIFIED;
}

static SCM gls_gpsm_swfile_position(SCM s_item)
{
	gpsm_item_t *item;
	SCM_ASSERT(gpsmitem_p(s_item)
		   && (item = scm2gpsmitem(s_item),
		       GPSM_ITEM_IS_SWFILE(item)), s_item,
		   SCM_ARG1, "gpsm-swfile-position");
	return gh_double2scm(gpsm_swfile_position(item));
}

static SCM gls_gpsm_swfile_set_position(SCM s_item, SCM s_pos)
{
	gpsm_item_t *item;
	SCM_ASSERT(gpsmitem_p(s_item)
		   && (item = scm2gpsmitem(s_item),
		       GPSM_ITEM_IS_SWFILE(item)), s_item,
		   SCM_ARG1, "gpsm-swfile-set-position!");
	SCM_ASSERT(gh_number_p(s_pos), s_pos,
		   SCM_ARG2, "gpsm-swfile-set-position!");
	gpsm_swfile_set_position((gpsm_swfile_t *)item,
				 gh_scm2double(s_pos));
	return SCM_UNSPECIFIED;
}

/*
 * Global control.
 */

static SCM gls_gpsm_init(SCM s_swapfile)
{
	char *swapfile;
	int res, swapfile_len;
	SCM_ASSERT(gh_string_p(s_swapfile), s_swapfile,
		   SCM_ARG1, "gpsm-init");
	swapfile = gh_scm2newstr(s_swapfile, &swapfile_len);
	res = gpsm_init(swapfile);
	if (res == -1)
		GLAME_THROW_ERRNO();
	return SCM_UNSPECIFIED;
}

static SCM gls_gpsm_set_max_saved_ops(SCM s_nr)
{
	SCM_ASSERT(gh_exact_p(s_nr), s_nr,
		   SCM_ARG1, "gpsm-set-max-saved-ops!");
	return gh_long2scm(gpsm_set_max_saved_ops(gh_scm2long(s_nr)));
}

static SCM gls_gpsm_sync()
{
	gpsm_sync();
	return SCM_UNSPECIFIED;
}

static SCM gls_gpsm_close()
{
	gpsm_close();
	return SCM_UNSPECIFIED;
}


/*
 * Structuring.
 */

static SCM gls_gpsm_root()
{
	return gpsmitem2scm(gpsm_root());
}

static SCM gls_gpsm_newswfile(SCM s_label)
{
	gpsm_item_t *item;
	char *label;
	int label_len;
	SCM_ASSERT(gh_string_p(s_label), s_label,
		   SCM_ARG1, "gpsm-newswfile");
	label = gh_scm2newstr(s_label, &label_len);
	item = (gpsm_item_t *)gpsm_newswfile(label);
	free(label);
	return gpsmitem2scm(item);
}

static SCM gls_gpsm_swfile_cow(SCM s_item)
{
	gpsm_item_t *item;
	SCM_ASSERT(gpsmitem_p(s_item)
		   && (item = scm2gpsmitem(s_item), GPSM_ITEM_IS_SWFILE(item)),
		   s_item, SCM_ARG1, "gpsm-swfile-cow");
	return gpsmitem2scm(gpsm_swfile_cow((gpsm_swfile_t *)item));
}

static SCM gls_gpsm_swfile_link(SCM s_item)
{
	gpsm_item_t *item;
	SCM_ASSERT(gpsmitem_p(s_item)
		   && (item = scm2gpsmitem(s_item), GPSM_ITEM_IS_SWFILE(item)),
		   s_item, SCM_ARG1, "gpsm-swfile-link");
	return gpsmitem2scm(gpsm_swfile_link((gpsm_swfile_t *)item));
}

static SCM gls_gpsm_newgrp(SCM s_label)
{
	gpsm_item_t *item;
	char *label;
	int label_len;
	SCM_ASSERT(gh_string_p(s_label), s_label,
		   SCM_ARG1, "gpsm-newgrp");
	label = gh_scm2newstr(s_label, &label_len);
	item = (gpsm_item_t *)gpsm_newgrp(label);
	free(label);
	return gpsmitem2scm(item);
}

static SCM gls_gpsm_item_destroy(SCM s_item)
{
	SCM_ASSERT(gpsmitem_p(s_item), s_item,
		   SCM_ARG1, "gpsm-item-destroy");
	gpsm_item_destroy(scm2gpsmitem(s_item));
	return SCM_UNSPECIFIED;
}

static SCM gls_gpsm_grp_is_hbox(SCM s_grp)
{
	gpsm_item_t *item;
	SCM_ASSERT(gpsmitem_p(s_grp)
		   && (item = scm2gpsmitem(s_grp), GPSM_ITEM_IS_GRP(item)),
		   s_grp, SCM_ARG1, "gpsm-grp-is-hbox?");
	if (gpsm_grp_is_hbox((gpsm_grp_t *)item))
		return SCM_BOOL_T;
	return SCM_BOOL_F;
}

static SCM gls_gpsm_grp_is_vbox(SCM s_grp)
{
	gpsm_item_t *item;
	SCM_ASSERT(gpsmitem_p(s_grp)
		   && (item = scm2gpsmitem(s_grp), GPSM_ITEM_IS_GRP(item)),
		   s_grp, SCM_ARG1, "gpsm-grp-is-vbox?");
	if (gpsm_grp_is_vbox((gpsm_grp_t *)item))
		return SCM_BOOL_T;
	return SCM_BOOL_F;
}

static SCM gls_gpsm_item_can_place(SCM s_grp, SCM s_item,
				   SCM s_hpos, SCM s_vpos)
{
	gpsm_item_t  *grp;
	SCM_ASSERT(gpsmitem_p(s_grp)
		   && (grp = scm2gpsmitem(s_grp), GPSM_ITEM_IS_GRP(grp)),
		   s_grp, SCM_ARG1, "gpsm-item-can-place?");
	SCM_ASSERT(gpsmitem_p(s_item), s_item,
		   SCM_ARG2, "gpsm-item-can-place?");
	SCM_ASSERT(gh_exact_p(s_hpos), s_hpos,
		   SCM_ARG3, "gpsm-item-can-place?");
	SCM_ASSERT(gh_exact_p(s_vpos), s_vpos,
		   SCM_ARG4, "gpsm-item-can-place?");
	if (gpsm_item_can_place((gpsm_grp_t *)grp, scm2gpsmitem(s_item),
				gh_scm2long(s_hpos), gh_scm2long(s_vpos)))
		return SCM_BOOL_T;
	return SCM_BOOL_F;
}

static SCM gls_gpsm_item_place(SCM s_grp, SCM s_item, SCM s_hpos, SCM s_vpos)
{
	gpsm_item_t  *grp;
	SCM_ASSERT(gpsmitem_p(s_grp)
		   && (grp = scm2gpsmitem(s_grp), GPSM_ITEM_IS_GRP(grp)),
		   s_grp, SCM_ARG1, "gpsm-item-place");
	SCM_ASSERT(gpsmitem_p(s_item), s_item,
		   SCM_ARG2, "gpsm-item-place");
	SCM_ASSERT(gh_exact_p(s_hpos), s_hpos,
		   SCM_ARG3, "gpsm-item-place");
	SCM_ASSERT(gh_exact_p(s_vpos), s_vpos,
		   SCM_ARG4, "gpsm-item-place");
	if (gpsm_item_place((gpsm_grp_t *)grp, scm2gpsmitem(s_item),
			    gh_scm2long(s_hpos), gh_scm2long(s_vpos)) == -1)
		GLAME_THROW();
	return SCM_UNSPECIFIED;
}

static SCM gls_gpsm_hbox_can_insert(SCM s_grp, SCM s_item,
				    SCM s_hpos, SCM s_vpos)
{
	gpsm_item_t  *grp;
	SCM_ASSERT(gpsmitem_p(s_grp)
		   && (grp = scm2gpsmitem(s_grp), GPSM_ITEM_IS_GRP(grp))
		   && (gpsm_grp_is_hbox((gpsm_grp_t *)grp)),
		   s_grp, SCM_ARG1, "gpsm-hbox-can-insert?");
	SCM_ASSERT(gpsmitem_p(s_item), s_item,
		   SCM_ARG2, "gpsm-hbox-can-insert?");
	SCM_ASSERT(gh_exact_p(s_hpos), s_hpos,
		   SCM_ARG3, "gpsm-hbox-can-insert?");
	SCM_ASSERT(gh_exact_p(s_vpos), s_vpos,
		   SCM_ARG4, "gpsm-hbox-can-insert?");
	if (gpsm_hbox_can_insert((gpsm_grp_t *)grp, scm2gpsmitem(s_item),
				 gh_scm2long(s_hpos), gh_scm2long(s_vpos)))
		return SCM_BOOL_T;
	return SCM_BOOL_F;
}

static SCM gls_gpsm_hbox_insert(SCM s_grp, SCM s_item, SCM s_hpos, SCM s_vpos)
{
	gpsm_item_t  *grp;
	SCM_ASSERT(gpsmitem_p(s_grp)
		   && (grp = scm2gpsmitem(s_grp), GPSM_ITEM_IS_GRP(grp))
		   && (gpsm_grp_is_hbox((gpsm_grp_t *)grp)),
		   s_grp, SCM_ARG1, "gpsm-hbox-insert");
	SCM_ASSERT(gpsmitem_p(s_item), s_item,
		   SCM_ARG2, "gpsm-hbox-insert");
	SCM_ASSERT(gh_exact_p(s_hpos), s_hpos,
		   SCM_ARG3, "gpsm-hbox-insert");
	SCM_ASSERT(gh_exact_p(s_vpos), s_vpos,
		   SCM_ARG4, "gpsm-hbox-insert");
	if (gpsm_hbox_insert((gpsm_grp_t *)grp, scm2gpsmitem(s_item),
			     gh_scm2long(s_hpos), gh_scm2long(s_vpos)) == -1)
		GLAME_THROW();
	return SCM_UNSPECIFIED;
}

static SCM gls_gpsm_vbox_can_insert(SCM s_grp, SCM s_item,
				    SCM s_hpos, SCM s_vpos)
{
	gpsm_item_t  *grp;
	SCM_ASSERT(gpsmitem_p(s_grp)
		   && (grp = scm2gpsmitem(s_grp), GPSM_ITEM_IS_GRP(grp))
		   && (gpsm_grp_is_vbox((gpsm_grp_t *)grp)),
		   s_grp, SCM_ARG1, "gpsm-vbox-can-insert?");
	SCM_ASSERT(gpsmitem_p(s_item), s_item,
		   SCM_ARG2, "gpsm-vbox-can-insert?");
	SCM_ASSERT(gh_exact_p(s_hpos), s_hpos,
		   SCM_ARG3, "gpsm-vbox-can-insert?");
	SCM_ASSERT(gh_exact_p(s_vpos), s_vpos,
		   SCM_ARG4, "gpsm-vbox-can-insert?");
	if (gpsm_vbox_can_insert((gpsm_grp_t *)grp, scm2gpsmitem(s_item),
				 gh_scm2long(s_hpos), gh_scm2long(s_vpos)))
		return SCM_BOOL_T;
	return SCM_BOOL_F;
}

static SCM gls_gpsm_vbox_insert(SCM s_grp, SCM s_item, SCM s_hpos, SCM s_vpos)
{
	gpsm_item_t  *grp;
	SCM_ASSERT(gpsmitem_p(s_grp)
		   && (grp = scm2gpsmitem(s_grp), GPSM_ITEM_IS_GRP(grp))
		   && (gpsm_grp_is_vbox((gpsm_grp_t *)grp)),
		   s_grp, SCM_ARG1, "gpsm-vbox-insert");
	SCM_ASSERT(gpsmitem_p(s_item), s_item,
		   SCM_ARG2, "gpsm-vbox-insert");
	SCM_ASSERT(gh_exact_p(s_hpos), s_hpos,
		   SCM_ARG3, "gpsm-vbox-insert");
	SCM_ASSERT(gh_exact_p(s_vpos), s_vpos,
		   SCM_ARG4, "gpsm-vbox-insert");
	if (gpsm_vbox_insert((gpsm_grp_t *)grp, scm2gpsmitem(s_item),
			     gh_scm2long(s_hpos), gh_scm2long(s_vpos)) == -1)
		GLAME_THROW();
	return SCM_UNSPECIFIED;
}

static SCM gls_gpsm_item_remove(SCM s_item)
{
	SCM_ASSERT(gpsmitem_p(s_item), s_item,
		   SCM_ARG1, "gpsm-item-remove");
	gpsm_item_remove(scm2gpsmitem(s_item));
	return SCM_UNSPECIFIED;
}

static SCM gls_gpsm_hbox_cut(SCM s_item)
{
	SCM_ASSERT(gpsmitem_p(s_item), s_item,
		   SCM_ARG1, "gpsm-hbox-cut");
	if (gpsm_hbox_cut(scm2gpsmitem(s_item)) == -1)
		GLAME_THROW();
	return SCM_UNSPECIFIED;
}

static SCM gls_gpsm_vbox_cut(SCM s_item)
{
	SCM_ASSERT(gpsmitem_p(s_item), s_item,
		   SCM_ARG1, "gpsm-vbox-cut");
	if (gpsm_vbox_cut(scm2gpsmitem(s_item)) == -1)
		GLAME_THROW();
	return SCM_UNSPECIFIED;
}


/*
 * Notification.
 */

static SCM gls_gpsm_notify_swapfile_change(SCM s_filename,
					   SCM s_pos, SCM s_length)
{
	SCM_ASSERT(gh_exact_p(s_filename), s_filename,
		   SCM_ARG1, "gpsm-notify-swapfile-change");
	SCM_ASSERT(gh_exact_p(s_pos), s_pos,
		   SCM_ARG2, "gpsm-notify-swapfile-change");
	SCM_ASSERT(gh_exact_p(s_length), s_length,
		   SCM_ARG3, "gpsm-notify-swapfile-change");
	gpsm_notify_swapfile_change(gh_scm2long(s_filename),
				    gh_scm2long(s_pos), gh_scm2long(s_length));
	return SCM_UNSPECIFIED;
}

static SCM gls_gpsm_notify_swapfile_cut(SCM s_filename,
					SCM s_pos, SCM s_length)
{
	SCM_ASSERT(gh_exact_p(s_filename), s_filename,
		   SCM_ARG1, "gpsm-notify-swapfile-cut");
	SCM_ASSERT(gh_exact_p(s_pos), s_pos,
		   SCM_ARG2, "gpsm-notify-swapfile-cut");
	SCM_ASSERT(gh_exact_p(s_length), s_length,
		   SCM_ARG3, "gpsm-notify-swapfile-cut");
	gpsm_notify_swapfile_cut(gh_scm2long(s_filename),
				 gh_scm2long(s_pos), gh_scm2long(s_length));
	return SCM_UNSPECIFIED;
}

static SCM gls_gpsm_notify_swapfile_insert(SCM s_filename,
					   SCM s_pos, SCM s_length)
{
	SCM_ASSERT(gh_exact_p(s_filename), s_filename,
		   SCM_ARG1, "gpsm-notify-swapfile-insert");
	SCM_ASSERT(gh_exact_p(s_pos), s_pos,
		   SCM_ARG2, "gpsm-notify-swapfile-insert");
	SCM_ASSERT(gh_exact_p(s_length), s_length,
		   SCM_ARG3, "gpsm-notify-swapfile-insert");
	gpsm_notify_swapfile_insert(gh_scm2long(s_filename),
				    gh_scm2long(s_pos), gh_scm2long(s_length));
	return SCM_UNSPECIFIED;
}

static SCM gls_gpsm_invalidate_swapfile(SCM s_filename)
{
	SCM_ASSERT(gh_exact_p(s_filename), s_filename,
		   SCM_ARG1, "gpsm-invalidate-swapfile");
	gpsm_invalidate_swapfile(gh_scm2long(s_filename));
	return SCM_UNSPECIFIED;
}


/*
 * Undo/redo.
 */

static SCM gls_gpsm_op_prepare(SCM s_item)
{
	SCM_ASSERT(gpsmitem_p(s_item), s_item,
		   SCM_ARG1, "gpsm-op-prepare");
	if (gpsm_op_prepare(scm2gpsmitem(s_item)) == -1)
		GLAME_THROW();
	return SCM_UNSPECIFIED;
}

static SCM gls_gpsm_op_can_undo(SCM s_item)
{
	SCM_ASSERT(gpsmitem_p(s_item), s_item,
		   SCM_ARG1, "gpsm-op-can-undo?");
	if (gpsm_op_can_undo(scm2gpsmitem(s_item)) == 1)
		return SCM_BOOL_T;
	return SCM_BOOL_F;
}

static SCM gls_gpsm_op_undo(SCM s_item)
{
	SCM_ASSERT(gpsmitem_p(s_item), s_item,
		   SCM_ARG1, "gpsm-op-undo");
	if (gpsm_op_undo(scm2gpsmitem(s_item)) == -1)
		GLAME_THROW();
	return SCM_UNSPECIFIED;
}

static SCM gls_gpsm_op_undo_and_forget(SCM s_item)
{
	SCM_ASSERT(gpsmitem_p(s_item), s_item,
		   SCM_ARG1, "gpsm-op-undo-and-forget");
	if (gpsm_op_undo_and_forget(scm2gpsmitem(s_item)) == -1)
		GLAME_THROW();
	return SCM_UNSPECIFIED;
}

static SCM gls_gpsm_op_can_redo(SCM s_item)
{
	SCM_ASSERT(gpsmitem_p(s_item), s_item,
		   SCM_ARG1, "gpsm-op-can-redo?");
	if (gpsm_op_can_redo(scm2gpsmitem(s_item)) == 1)
		return SCM_BOOL_T;
	return SCM_BOOL_F;
}

static SCM gls_gpsm_op_redo(SCM s_item)
{
	SCM_ASSERT(gpsmitem_p(s_item), s_item,
		   SCM_ARG1, "gpsm-op-redo");
	if (gpsm_op_redo(scm2gpsmitem(s_item)) == -1)
		GLAME_THROW();
	return SCM_UNSPECIFIED;
}

static SCM gls_gpsm_op_redo_and_forget(SCM s_item)
{
	SCM_ASSERT(gpsmitem_p(s_item), s_item,
		   SCM_ARG1, "gpsm-op-redo-and-forget");
	if (gpsm_op_redo_and_forget(scm2gpsmitem(s_item)) == -1)
		GLAME_THROW();
	return SCM_UNSPECIFIED;
}

static SCM gls_gpsm_op_forget(SCM s_item)
{
	SCM_ASSERT(gpsmitem_p(s_item), s_item,
		   SCM_ARG1, "gpsm-op-forget");
	if (gpsm_op_forget(scm2gpsmitem(s_item)) == -1)
		GLAME_THROW();
	return SCM_UNSPECIFIED;
}




int glscript_init_gpsm()
{
	/* Register gpsmitem, gpsmgrp and gpsmswfile SMOBs to guile. */
	gpsmitem_smob_tag = scm_make_smob_type("gpsmitem",
					       sizeof(struct pointer_smob));
	scm_set_smob_print(gpsmitem_smob_tag, print_pointer);
	scm_set_smob_equalp(gpsmitem_smob_tag, equalp_pointer);

	/* gpsm types access. */
	gh_new_procedure1_0("gpsm-item?", gls_gpsm_is_item);
	gh_new_procedure1_0("gpsm-item-parent", gls_gpsm_item_parent);
	gh_new_procedure1_0("gpsm-item-label", gls_gpsm_item_label);
	gh_new_procedure2_0("gpsm-item-set-label!", gls_gpsm_item_set_label);
	gh_new_procedure1_0("gpsm-item-hposition", gls_gpsm_item_hposition);
	gh_new_procedure1_0("gpsm-item-vposition", gls_gpsm_item_vposition);
	gh_new_procedure1_0("gpsm-item-hsize", gls_gpsm_item_hsize);
	gh_new_procedure1_0("gpsm-item-vsize", gls_gpsm_item_vsize);
	gh_new_procedure1_0("gpsm-grp?", gls_gpsm_is_grp);
	gh_new_procedure1_0("gpsm-grp-items", gls_gpsm_grp_items);
	gh_new_procedure1_0("gpsm-swfile?", gls_gpsm_is_swfile);
	gh_new_procedure1_0("gpsm-swfile-filename", gls_gpsm_swfile_filename);
	gh_new_procedure1_0("gpsm-swfile-samplerate",
			    gls_gpsm_swfile_samplerate);
	gh_new_procedure2_0("gpsm-swfile-set-samplerate!",
			    gls_gpsm_swfile_set_samplerate);
	gh_new_procedure1_0("gpsm-swfile-position", gls_gpsm_swfile_position);
	gh_new_procedure2_0("gpsm-swfile-set-position!",
			    gls_gpsm_swfile_set_position);

	/* gpsm global control. */
	gh_new_procedure1_0("gpsm-init", gls_gpsm_init);
	gh_new_procedure1_0("gpsm-set-max-saved-ops!",
			    gls_gpsm_set_max_saved_ops);
	gh_new_procedure0_0("gpsm-sync", gls_gpsm_sync);
	gh_new_procedure0_0("gpsm-close", gls_gpsm_close);

	/* gpsm structuring. */
	gh_new_procedure0_0("gpsm-root", gls_gpsm_root);
	gh_new_procedure1_0("gpsm-newswfile", gls_gpsm_newswfile);
	gh_new_procedure1_0("gpsm-swfile-cow", gls_gpsm_swfile_cow);
	gh_new_procedure1_0("gpsm-swfile-link", gls_gpsm_swfile_link);
	gh_new_procedure1_0("gpsm-newgrp", gls_gpsm_newgrp);
	gh_new_procedure1_0("gpsm-item-destroy", gls_gpsm_item_destroy);
	gh_new_procedure1_0("gpsm-grp-hbox?", gls_gpsm_grp_is_hbox);
	gh_new_procedure1_0("gpsm-grp-vbox?", gls_gpsm_grp_is_vbox);
	gh_new_procedure4_0("gpsm-item-can-place?", gls_gpsm_item_can_place);
	gh_new_procedure4_0("gpsm-item-place", gls_gpsm_item_place);
	gh_new_procedure4_0("gpsm-hbox-can-insert?", gls_gpsm_hbox_can_insert);
	gh_new_procedure4_0("gpsm-hbox-insert", gls_gpsm_hbox_insert);
	gh_new_procedure4_0("gpsm-vbox-can-insert?", gls_gpsm_vbox_can_insert);
	gh_new_procedure4_0("gpsm-vbox-insert", gls_gpsm_vbox_insert);
	gh_new_procedure1_0("gpsm-item-remove", gls_gpsm_item_remove);
	gh_new_procedure1_0("gpsm-hbox-cut", gls_gpsm_hbox_cut);
	gh_new_procedure1_0("gpsm-vbox-cut", gls_gpsm_vbox_cut);

	/* gpsm notification. */
	gh_new_procedure3_0("gpsm-notify-swapfile-change",
			    gls_gpsm_notify_swapfile_change);
	gh_new_procedure3_0("gpsm-notify-swapfile-cut",
			    gls_gpsm_notify_swapfile_cut);
	gh_new_procedure3_0("gpsm-notify-swapfile-insert",
			    gls_gpsm_notify_swapfile_insert);
	gh_new_procedure1_0("gpsm-invalidate-swapfile",
			    gls_gpsm_invalidate_swapfile);

	/* gpsm undo/redo. */
	gh_new_procedure1_0("gpsm-op-prepare", gls_gpsm_op_prepare);
	gh_new_procedure1_0("gpsm-op-can-undo?", gls_gpsm_op_can_undo);
	gh_new_procedure1_0("gpsm-op-undo", gls_gpsm_op_undo);
	gh_new_procedure1_0("gpsm-op-undo-and-forget",
			    gls_gpsm_op_undo_and_forget);
	gh_new_procedure1_0("gpsm-op-can-redo?", gls_gpsm_op_can_redo);
	gh_new_procedure1_0("gpsm-op-redo", gls_gpsm_op_redo);
	gh_new_procedure1_0("gpsm-op-redo-and-forget",
			    gls_gpsm_op_redo_and_forget);
	gh_new_procedure1_0("gpsm-op-forget", gls_gpsm_op_forget);

	/* FIXME: search, collect, flatten and transform tobe done
	 * in scheme.
	 */

	return 0;
}
