/*
 * glscript_gpsm.c
 *
 * Copyright (C) 2001, 2002, 2004 Richard Guenther
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

long gpsmitem_smob_tag = 0;
struct gpsmitem_smob {
	gpsm_item_t *item;
};
#define SCM2GPSMITEMSMOB(s) ((struct gpsmitem_smob *)SCM_SMOB_DATA(s))
SCM gpsmitem2scm(gpsm_item_t *item);
gpsm_item_t *scm2gpsmitem(SCM gpsmitem_smob);

static size_t free_gpsmitem(SCM gpsmitem_smob)
{
#if 0
	struct gpsmitem_smob *item = SCM2GPSMITEMSMOB(gpsmitem_smob);

	/* Delete the item if it is not the root item and
	 * has no parent. -- NOT. */
	if (item->item
	    && item->item != gpsm_root()
	    && !gpsm_item_parent(item->item)) {
		DPRINTF("GCing gpsm item %p (%s)\n", item->item,
			gpsm_item_label(item->item));
		gpsm_item_destroy(item->item);
		item->item = NULL;
	}
#endif

	return sizeof(struct gpsmitem_smob);
}

static int print_gpsmitem(SCM gpsmitem_smob, SCM port, scm_print_state *pstate)
{
	struct gpsmitem_smob *item = SCM2GPSMITEMSMOB(gpsmitem_smob);
	char buf[256];

	if (!item->item) {
		snprintf(buf, 255, "#< destroyed gpsm item >");
		scm_puts(buf, port);
	} else if (GPSM_ITEM_IS_SWFILE(item->item)) {
		snprintf(buf, 255, "#< gpsm-swfile \"%s\" file=%li rate=%i pos=%.3f (+%li) > ",
			 gpsm_item_label(item->item),
			 gpsm_swfile_filename(item->item),
			 gpsm_swfile_samplerate(item->item),
			 gpsm_swfile_position(item->item),
			 gpsm_item_hsize(item->item));
		scm_puts(buf, port);
	} else if (GPSM_ITEM_IS_GRP(item->item)) {
		snprintf(buf, 255, "#< gpsm-grp \"%s\" (+%li, +%li) > ",
			 gpsm_item_label(item->item),
			 gpsm_item_hsize(item->item),
			 gpsm_item_vsize(item->item));
		scm_puts(buf, port);
	}

	return 1;
}

static SCM equalp_gpsmitem(SCM gpsmitem_smob1, SCM gpsmitem_smob2)
{
	struct gpsmitem_smob *item1 = SCM2GPSMITEMSMOB(gpsmitem_smob1);
	struct gpsmitem_smob *item2 = SCM2GPSMITEMSMOB(gpsmitem_smob2);

	if (item1->item == item2->item)
		return SCM_BOOL_T;
	return SCM_BOOL_F;
}

SCM gpsmitem2scm(gpsm_item_t *item)
{
	struct gpsmitem_smob *smob;
	SCM gpsmitem_smob;

	if (!item)
		GLAME_THROW();

	smob = (struct gpsmitem_smob *)malloc(sizeof(struct gpsmitem_smob));
	smob->item = item;

	SCM_NEWSMOB(gpsmitem_smob, gpsmitem_smob_tag, smob);

	return gpsmitem_smob;
}

gpsm_item_t *scm2gpsmitem(SCM gpsmitem_smob)
{
	SCM_ASSERT(gpsmitem_p(gpsmitem_smob),
		   gpsmitem_smob, SCM_ARG1, "scm2gpsmitem");
	return SCM2GPSMITEMSMOB(gpsmitem_smob)->item;
}

void scminvalidategpsmitem(SCM gpsmitem_smob)
{
	struct gpsmitem_smob *item = SCM2GPSMITEMSMOB(gpsmitem_smob);

	SCM_ASSERT(gpsmitem_p(gpsmitem_smob),
		   gpsmitem_smob, SCM_ARG1, "scminvalidategpsmitem");

	item->item = NULL;
}




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
	return gpsmitem2scm((gpsm_item_t *)gpsm_item_parent(item));
}

static SCM gls_gpsm_item_label(SCM s_item)
{
	gpsm_item_t *item;
	SCM_ASSERT(gpsmitem_p(s_item), s_item,
		   SCM_ARG1, "gpsm-item-label");
	item = scm2gpsmitem(s_item);
	return scm_makfrom0str(gpsm_item_label(item));
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
	return scm_long2num(gpsm_item_hposition(item));
}

static SCM gls_gpsm_item_vposition(SCM s_item)
{
	gpsm_item_t *item;
	SCM_ASSERT(gpsmitem_p(s_item), s_item,
		   SCM_ARG1, "gpsm-item-vposition");
	item = scm2gpsmitem(s_item);
	return scm_long2num(gpsm_item_vposition(item));
}

static SCM gls_gpsm_item_hsize(SCM s_item)
{
	gpsm_item_t *item;
	SCM_ASSERT(gpsmitem_p(s_item), s_item,
		   SCM_ARG1, "gpsm-item-hsize");
	item = scm2gpsmitem(s_item);
	return scm_long2num(gpsm_item_hsize(item));
}

static SCM gls_gpsm_item_vsize(SCM s_item)
{
	gpsm_item_t *item;
	SCM_ASSERT(gpsmitem_p(s_item), s_item,
		   SCM_ARG1, "gpsm-item-vsize");
	item = scm2gpsmitem(s_item);
	return scm_long2num(gpsm_item_vsize(item));
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
	SCM s_items = SCM_EOL;
	SCM_ASSERT(gpsmitem_p(s_item), s_item,
		   SCM_ARG1, "gpsm-grp-items");
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
	return scm_long2num(gpsm_swfile_filename(item));
}

static SCM gls_gpsm_swfile_samplerate(SCM s_item)
{
	gpsm_item_t *item;
	SCM_ASSERT(gpsmitem_p(s_item)
		   && (item = scm2gpsmitem(s_item),
		       GPSM_ITEM_IS_SWFILE(item)), s_item,
		   SCM_ARG1, "gpsm-swfile-samplerate");
	return scm_long2num(gpsm_swfile_samplerate(item));
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
				   glame_scm2long(s_rate));
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

static SCM gls_gpsm_init(SCM s_swapfile, SCM s_maxundo)
{
	char *swapfile;
	int res, swapfile_len, maxundo;
	SCM_ASSERT(gh_string_p(s_swapfile), s_swapfile,
		   SCM_ARG1, "gpsm-init");
	SCM_ASSERT(gh_exact_p(s_maxundo), s_maxundo,
		   SCM_ARG2, "gpsm-init");
	swapfile = gh_scm2newstr(s_swapfile, &swapfile_len);
	maxundo = glame_scm2long(s_maxundo);
	res = gpsm_init(swapfile, maxundo);
	if (res == -1)
		GLAME_THROW_ERRNO();
	return SCM_UNSPECIFIED;
}

static SCM gls_gpsm_set_max_saved_ops(SCM s_nr)
{
	SCM_ASSERT(gh_exact_p(s_nr), s_nr,
		   SCM_ARG1, "gpsm-set-max-saved-ops!");
	return scm_long2num(gpsm_set_max_saved_ops(glame_scm2long(s_nr)));
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
	return gpsmitem2scm((gpsm_item_t *)gpsm_root());
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
	return gpsmitem2scm((gpsm_item_t *)gpsm_swfile_cow((gpsm_swfile_t *)item));
}

static SCM gls_gpsm_swfile_link(SCM s_item)
{
	gpsm_item_t *item;
	SCM_ASSERT(gpsmitem_p(s_item)
		   && (item = scm2gpsmitem(s_item), GPSM_ITEM_IS_SWFILE(item)),
		   s_item, SCM_ARG1, "gpsm-swfile-link");
	return gpsmitem2scm((gpsm_item_t *)gpsm_swfile_link((gpsm_swfile_t *)item));
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
	gpsm_item_t *item;
	SCM_ASSERT(gpsmitem_p(s_item), s_item,
		   SCM_ARG1, "gpsm-item-destroy");
	item = scm2gpsmitem(s_item);
	scminvalidategpsmitem(s_item);
	gpsm_item_destroy(item);
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
				glame_scm2long(s_hpos), glame_scm2long(s_vpos)))
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
			    glame_scm2long(s_hpos), glame_scm2long(s_vpos)) == -1)
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
				 glame_scm2long(s_hpos), glame_scm2long(s_vpos)))
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
			     glame_scm2long(s_hpos), glame_scm2long(s_vpos)) == -1)
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
				 glame_scm2long(s_hpos), glame_scm2long(s_vpos)))
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
			     glame_scm2long(s_hpos), glame_scm2long(s_vpos)) == -1)
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
	gpsm_notify_swapfile_change(glame_scm2long(s_filename),
				    glame_scm2long(s_pos), glame_scm2long(s_length));
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
	gpsm_notify_swapfile_cut(glame_scm2long(s_filename),
				 glame_scm2long(s_pos), glame_scm2long(s_length));
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
	gpsm_notify_swapfile_insert(glame_scm2long(s_filename),
				    glame_scm2long(s_pos), glame_scm2long(s_length));
	return SCM_UNSPECIFIED;
}

static SCM gls_gpsm_invalidate_swapfile(SCM s_filename)
{
	SCM_ASSERT(gh_exact_p(s_filename), s_filename,
		   SCM_ARG1, "gpsm-invalidate-swapfile");
	gpsm_invalidate_swapfile(glame_scm2long(s_filename));
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
					       sizeof(struct gpsmitem_smob));
	scm_set_smob_free(gpsmitem_smob_tag, free_gpsmitem);
	scm_set_smob_print(gpsmitem_smob_tag, print_gpsmitem);
	scm_set_smob_equalp(gpsmitem_smob_tag, equalp_gpsmitem);

	/* gpsm types access. */
	glame_reg_export ("gpsm-item?", 1, 0, 0, gls_gpsm_is_item);
	glame_reg_export ("gpsm-item-parent", 1, 0, 0, gls_gpsm_item_parent);
	glame_reg_export ("gpsm-item-label", 1, 0, 0, gls_gpsm_item_label);
	glame_reg_export ("gpsm-item-set-label!", 2, 0, 0, 
			  gls_gpsm_item_set_label);
	glame_reg_export ("gpsm-item-hposition", 1, 0, 0, 
			  gls_gpsm_item_hposition);
	glame_reg_export ("gpsm-item-vposition", 1, 0, 0, 
			  gls_gpsm_item_vposition);
	glame_reg_export ("gpsm-item-hsize", 1, 0, 0, gls_gpsm_item_hsize);
	glame_reg_export ("gpsm-item-vsize", 1, 0, 0, gls_gpsm_item_vsize);
	glame_reg_export ("gpsm-grp?", 1, 0, 0, gls_gpsm_is_grp);
	glame_reg_export ("gpsm-grp-items", 1, 0, 0, gls_gpsm_grp_items);
	glame_reg_export ("gpsm-swfile?", 1, 0, 0, gls_gpsm_is_swfile);
	glame_reg_export ("gpsm-swfile-filename", 1, 0, 0, 
			  gls_gpsm_swfile_filename);
	glame_reg_export ("gpsm-swfile-samplerate",
			    1, 0, 0, gls_gpsm_swfile_samplerate);
	glame_reg_export ("gpsm-swfile-set-samplerate!",
			    2, 0, 0, gls_gpsm_swfile_set_samplerate);
	glame_reg_export ("gpsm-swfile-position", 1, 0, 0, 
			  gls_gpsm_swfile_position);
	glame_reg_export ("gpsm-swfile-set-position!",
			    2, 0, 0, gls_gpsm_swfile_set_position);

	/* gpsm global control. */
	glame_reg_export ("gpsm-init", 1, 0, 0, gls_gpsm_init);
	glame_reg_export ("gpsm-set-max-saved-ops!",
			    1, 0, 0, gls_gpsm_set_max_saved_ops);
	glame_reg_export ("gpsm-sync", 0, 0, 0, gls_gpsm_sync);
	glame_reg_export ("gpsm-close", 0, 0, 0, gls_gpsm_close);

	/* gpsm structuring. */
	glame_reg_export ("gpsm-root", 0, 0, 0, gls_gpsm_root);
	glame_reg_export ("gpsm-newswfile", 1, 0, 0, gls_gpsm_newswfile);
	glame_reg_export ("gpsm-swfile-cow", 1, 0, 0, gls_gpsm_swfile_cow);
	glame_reg_export ("gpsm-swfile-link", 1, 0, 0, gls_gpsm_swfile_link);
	glame_reg_export ("gpsm-newgrp", 1, 0, 0, gls_gpsm_newgrp);
	glame_reg_export ("gpsm-item-destroy", 1, 0, 0, gls_gpsm_item_destroy);
	glame_reg_export ("gpsm-grp-hbox?", 1, 0, 0, gls_gpsm_grp_is_hbox);
	glame_reg_export ("gpsm-grp-vbox?", 1, 0, 0, gls_gpsm_grp_is_vbox);
	glame_reg_export ("gpsm-item-can-place?", 4, 0, 0, 
			  gls_gpsm_item_can_place);
	glame_reg_export ("gpsm-item-place", 4, 0, 0, gls_gpsm_item_place);
	glame_reg_export ("gpsm-hbox-can-insert?", 4, 0, 0, 
			  gls_gpsm_hbox_can_insert);
	glame_reg_export ("gpsm-hbox-insert", 4, 0, 0, gls_gpsm_hbox_insert);
	glame_reg_export ("gpsm-vbox-can-insert?", 4, 0, 0, 
			  gls_gpsm_vbox_can_insert);
	glame_reg_export ("gpsm-vbox-insert", 4, 0, 0, gls_gpsm_vbox_insert);
	glame_reg_export ("gpsm-item-remove", 1, 0, 0, gls_gpsm_item_remove);
	glame_reg_export ("gpsm-hbox-cut", 1, 0, 0, gls_gpsm_hbox_cut);
	glame_reg_export ("gpsm-vbox-cut", 1, 0, 0, gls_gpsm_vbox_cut);

	/* gpsm notification. */
	glame_reg_export ("gpsm-notify-swapfile-change",
			    3, 0, 0, gls_gpsm_notify_swapfile_change);
	glame_reg_export ("gpsm-notify-swapfile-cut",
			    3, 0, 0, gls_gpsm_notify_swapfile_cut);
	glame_reg_export ("gpsm-notify-swapfile-insert",
			    3, 0, 0, gls_gpsm_notify_swapfile_insert);
	glame_reg_export ("gpsm-invalidate-swapfile",
			    1, 0, 0, gls_gpsm_invalidate_swapfile);

	/* gpsm undo/redo. */
	glame_reg_export ("gpsm-op-prepare", 1, 0, 0, gls_gpsm_op_prepare);
	glame_reg_export ("gpsm-op-can-undo?", 1, 0, 0, gls_gpsm_op_can_undo);
	glame_reg_export ("gpsm-op-undo", 1, 0, 0, gls_gpsm_op_undo);
	glame_reg_export ("gpsm-op-undo-and-forget",
			    1, 0, 0, gls_gpsm_op_undo_and_forget);
	glame_reg_export ("gpsm-op-can-redo?", 1, 0, 0, gls_gpsm_op_can_redo);
	glame_reg_export ("gpsm-op-redo", 1, 0, 0, gls_gpsm_op_redo);
	glame_reg_export ("gpsm-op-redo-and-forget",
			    1, 0, 0, gls_gpsm_op_redo_and_forget);
	glame_reg_export ("gpsm-op-forget", 1, 0, 0, gls_gpsm_op_forget);

	/* FIXME: search, collect, flatten and transform tobe done
	 * in scheme.
	 */

	return 0;
}
