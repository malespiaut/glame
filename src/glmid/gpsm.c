/*
 * gpsm.h
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

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <xmlmemory.h>
#include <parser.h>
#include "glame_types.h"
#include "swapfile.h"
#include "gpsm.h"
#include "hash.h"


static gpsm_grp_t *root = NULL;
HASH(swfile, gpsm_swfile_t, 8,
     (swfile->filename == filename),
     (filename),
     (swfile->filename),
     long filename)


static gpsm_item_t *gpsm_newitem(int type);


/*
 * XML input/output helpers.
 */

static void dump_tree(gpsm_grp_t *tree, xmlNodePtr node);
static void dump_item(gpsm_item_t *item, xmlNodePtr node)
{
	xmlNodePtr child;
	char s[256];

	if (GPSM_ITEM_IS_GRP(item)) {
		gpsm_grp_t *group = (gpsm_grp_t *)item;
		child = xmlNewChild(node, NULL, "group", NULL);
		if (!list_empty(&group->items))
			dump_tree(group, child);
	} else if (GPSM_ITEM_IS_SWFILE(item)) {
		gpsm_swfile_t *swfile = (gpsm_swfile_t *)item;
		child = xmlNewChild(node, NULL, "file", NULL);
		snprintf(s, 255, "%li", swfile->filename);
		xmlSetProp(child, "fd", s);
		snprintf(s, 255, "%i", swfile->samplerate);
		xmlSetProp(child, "rate", s);
		snprintf(s, 255, "%.3f", swfile->position);
		xmlSetProp(child, "position", s);
	} else {
		PANIC("GPSM item neither neither group nor swapfile.");
	}
	xmlSetProp(child, "label", item->label);
	snprintf(s, 255, "%li", item->hposition);
	xmlSetProp(child, "hpos", s);
	snprintf(s, 255, "%li", item->vposition);
	xmlSetProp(child, "vpos", s);
}
static void dump_tree(gpsm_grp_t *tree, xmlNodePtr node)
{
	gpsm_item_t *item;

	if (!tree)
		return;
	list_foreach(&tree->items, gpsm_item_t, list, item)
		dump_item(item, node);
}

static void insert_node(gpsm_grp_t *tree, xmlNodePtr node);
static void insert_childs(gpsm_grp_t *tree, xmlNodePtr node)
{
#ifndef xmlChildrenNode
        node = node->childs;
#else
        node = node->xmlChildrenNode;
#endif
        if (!node)
		return;

	while (node) {
		insert_node(tree, node);
		node = node->next;
	}
}
static void insert_node(gpsm_grp_t *tree, xmlNodePtr node)
{
        gpsm_item_t *item;
	char *c, *ilabel;
	long ihposition, ivposition;

	if (!(c = xmlGetProp(node, "label")))
		c = "(unnamed)";
	ilabel = strdup(c);

	if (strcmp(node->name, "file") == 0) {
		gpsm_swfile_t *swfile;
		long ifd;
		float iposition;
		struct sw_stat st;
		int irate, fd;

		/* Extract file information. */
		if (!(c = xmlGetProp(node, "fd")))
			c = "-1";
		if (sscanf(c, "%li", &ifd) != 1)
			ifd = -1;
		if (!(c = xmlGetProp(node, "rate")))
			c = "44100";
		if (sscanf(c, "%i", &irate) != 1)
			irate = 44100;
		if (!(c = xmlGetProp(node, "position")))
			c = "0.0";
		if (sscanf(c, "%f", &iposition) != -1)
			iposition = 0.0;

		/* Check, if the file is really there (and update info) */
		if ((fd = sw_open(ifd, O_RDONLY, TXN_NONE)) != -1) {
			sw_fstat(fd, &st);
			sw_close(fd);
		} else {
			DPRINTF("%s does not exist\n", ilabel);
			return;
		}

		swfile = (gpsm_swfile_t *)item = gpsm_newitem(GPSM_ITEM_TYPE_SWFILE);
		swfile->filename = ifd;
		swfile->samplerate = irate;
		swfile->position = iposition;
		swfile->item.hsize = st.size/SAMPLE_SIZE;
		swfile->item.vsize = 1;
		hash_add_swfile(swfile);

	} else if (strcmp(node->name, "group") == 0) {
		item = gpsm_newitem(GPSM_ITEM_TYPE_GRP);

	} else if (strcmp(node->name, "swapfile") == 0) {
		DERROR("Illegal <swapfile> tag position");
	} else {
		return;
	}

	item->label = ilabel;
	if (!(c = xmlGetProp(node, "hpos")))
		c = "0";
	if (sscanf(c, "%li", &ihposition) != 1)
		ihposition = 0;
	if (!(c = xmlGetProp(node, "vpos")))
		c = "0";
	if (sscanf(c, "%li", &ivposition) != 1)
		ivposition = -1;

	gpsm_grp_insert(tree, item, ihposition, ivposition);
	if (GPSM_ITEM_IS_GRP(item))
		insert_childs((gpsm_grp_t *)item, node);
}

/* Scan the swapfile and add all non-xmled files to a seperate
 * group. */
static void scan_swap()
{
	gpsm_swfile_t *swfile;
	gpsm_grp_t *grp;
	SWDIR *dir;
	struct sw_stat st;
	long name;
	int fd;

	/* Add unknown group and iterate through all swapfiles adding those
	 * not already contained in the tree. */
	if (!(grp = gpsm_find_grp_label(gpsm_root(), NULL, "[unrecognized tracks]"))) {
		grp = gpsm_newgrp("[unrecognized tracks]");
		gpsm_grp_insert(gpsm_root(), (gpsm_item_t *)grp, 0, 100000);
	}
	dir = sw_opendir();
	while ((name = sw_readdir(dir)) != -1) {
		if (name == 0)
			continue;
		if (gpsm_find_swfile_filename(gpsm_root(), NULL, name))
			continue;
		fd = sw_open(name, O_RDONLY, TXN_NONE);
		sw_fstat(fd, &st);
		sw_close(fd);
		swfile = (gpsm_swfile_t *)gpsm_newitem(GPSM_ITEM_TYPE_SWFILE);
		swfile->item.label = strdup("(unnamed)");
		swfile->item.hsize = st.size/SAMPLE_SIZE;
		swfile->item.vsize = 1;
		swfile->filename = name;
		swfile->samplerate = 44100;
		swfile->position = 0.0;

		hash_add_swfile(swfile);
		gpsm_grp_insert(grp, (gpsm_item_t *)swfile, 0, -1);
	}
	sw_closedir(dir);

	/* Check if the unknown group is empty and if so, remove it. */
	if (list_empty(&grp->items))
		gpsm_item_destroy((gpsm_item_t *)grp);
}


/*
 * Initialization API.
 */

int gpsm_init(const char *swapfile)
{
	xmlDocPtr doc;
	char *xml;
	int fd;

	if (root)
		return -1;
	if (swapfile_open(swapfile, 0) == -1) {
		if (errno != EBUSY) {
			perror("ERROR: Unable to open swap");
			return -1;
		}
		fprintf(stderr, "WARNING: Unclean swap - running fsck\n");
		if (swapfile_fsck(swapfile) == -1) {
			perror("ERROR: Fsck failed");
			return -1;
		}
		fprintf(stderr, "WARNING: Fsck successful\n");
		if (swapfile_open(swapfile, 0) == -1) {
			perror("ERROR: Still cannot open swap");
			return -1;
		}
	}

	/* Read swapfile 0 into a character buffer for libxml. */
	if ((fd = sw_open(0, O_RDONLY, TXN_NONE)) != -1) {
		struct sw_stat st;
		if (sw_fstat(fd, &st) == -1
		    || !(xml = malloc(st.size+1))) {
			sw_close(fd);
			return -1;
		}
		if (sw_read(fd, xml, st.size) != st.size) {
			sw_close(fd);
			free(xml);
			return -1;
		}
		xml[st.size] = '\0';
		sw_close(fd);
	} else {
		/* Seems to be empty swapfile - use "default" xml. */
		xml = strdup(""
"<?xml version=\"1.0\"?>\n"
"<swapfile>\n"
"</swapfile>");
	}

	/* Try to parse the xml string. */
	if (!(doc = xmlParseMemory(xml, strlen(xml)))) {
		free(xml);
		return -1;
	}

	/* Create the tree root group and recurse down the xml tree. */
        root = (gpsm_grp_t *)gpsm_newitem(GPSM_ITEM_TYPE_GRP);
	root->item.label = strdup("(root)");
	root->item.hposition = 0;
	root->item.vposition = 0;
	root->item.hsize = 0;
	root->item.vsize = 0;
        insert_childs(root, xmlDocGetRootElement(doc));

	/* Search for not xml-ed swapfile. */
	scan_swap();

	free(xml);
	xmlFreeDoc(doc);

	return 0;
}

void gpsm_sync()
{
	xmlDocPtr doc;
	xmlNodePtr docroot;
	xmlChar *xml;
	int size, fd;

	/* Sync the swapfile. */
	swapfile_sync();

	/* Save swapfile_tree as xml into swapfile 0 */
	doc = xmlNewDoc("1.0");
	docroot = xmlNewNode(NULL, "swapfile");
	dump_tree(root, docroot);
	xmlDocSetRootElement(doc, docroot);
	xmlDocDumpMemory(doc, &xml, &size);
	DPRINTF("%i bytes xml\n %s\n", size, xml);
	fd = sw_open(0, O_RDWR|O_CREAT|O_TRUNC, TXN_NONE);
	sw_write(fd, xml, size);
	sw_close(fd);
	free(xml);
	xmlFreeDoc(doc);
}

void gpsm_close()
{
	if (!root)
		return;

	/* Sync the swapfile and the xml representation and
	 * close the swapfile. */
	gpsm_sync();
	swapfile_close();

	/* Delete the gpsm tree (from memory, the swapfile functions
	 * have no effect with closed swapfile). */
	gpsm_item_destroy((gpsm_item_t *)root);
	root = NULL;
}



/*
 * Item constructors / destructors.
 */

static gpsm_item_t *gpsm_newitem(int type)
{
	gpsm_item_t *item;

	/* Allocate item. */
	switch (type) {
	case GPSM_ITEM_TYPE_GRP:
		item = (gpsm_item_t *)malloc(sizeof(gpsm_grp_t));
		break;
	case GPSM_ITEM_TYPE_SWFILE:
		item = (gpsm_item_t *)malloc(sizeof(gpsm_swfile_t));
		break;
	default:
		return NULL;
	}
	if (!item)
		return NULL;

	/* Initialize item. */
	INIT_LIST_HEAD(&item->list);
	item->parent = NULL;
	INIT_GLSIG_EMITTER(&item->emitter);
	item->type = type;
	item->label = NULL;
	item->hposition = 0;
	item->vposition = 0;
	item->hsize = 0;
	item->vsize = 0;
	if (GPSM_ITEM_IS_GRP(item))
		INIT_LIST_HEAD(&((gpsm_grp_t *)item)->items);
	else if (GPSM_ITEM_IS_SWFILE(item)) {
		hash_init_swfile((gpsm_swfile_t *)item);
		item->hsize = 0;
		item->vsize = 1;
		((gpsm_swfile_t *)item)->filename = -1;
	}

	return item;
}


gpsm_swfile_t *gpsm_newswfile(const char *label)
{
	gpsm_swfile_t *swfile;
	swfd_t fd;

	if (!label)
		return NULL;

	swfile = (gpsm_swfile_t *)gpsm_newitem(GPSM_ITEM_TYPE_SWFILE);
	swfile->item.label = strdup(label);
	swfile->item.hposition = 0;
	swfile->item.vposition = 0;
	swfile->item.hsize = 0;
	swfile->item.vsize = 1;
	while ((fd = sw_open((swfile->filename = rand()),
			     O_RDWR|O_CREAT|O_EXCL, TXN_NONE)) == -1)
		;
	sw_close(fd);
	swfile->samplerate = GLAME_DEFAULT_SAMPLERATE;
	swfile->position = 0.0;
	hash_add_swfile(swfile);

	return swfile;
}

gpsm_swfile_t *gpsm_swfile_cow(gpsm_swfile_t *source)
{
	gpsm_swfile_t *swfile;
	struct sw_stat st;
	swfd_t sfd, dfd;
	int res;

	if (!source)
		return NULL;

	if ((sfd = sw_open(source->filename, O_RDONLY, TXN_NONE)) == -1)
		return NULL;
	if (sw_fstat(sfd, &st) == -1) {
		sw_close(sfd);
		return NULL;
	}

	swfile = (gpsm_swfile_t *)gpsm_newitem(GPSM_ITEM_TYPE_SWFILE);
	while ((dfd = sw_open((swfile->filename = rand()),
			      O_RDWR|O_CREAT|O_EXCL, TXN_NONE)) == -1)
		;

	if ((res = sw_ftruncate(dfd, st.size)) == -1
	    || (res = sw_sendfile(dfd, sfd, st.size, 0)) == -1)
		;
	sw_close(sfd);
	sw_close(dfd);
	if (res == -1) {
		sw_unlink(swfile->filename);
		free(swfile);
		return NULL;
	}

	swfile->item.label = strdup(source->item.label);
	swfile->item.hposition = 0;
	swfile->item.vposition = 0;
	swfile->item.hsize = source->item.hsize;
	swfile->item.vsize = source->item.vsize;
	swfile->samplerate = source->samplerate;
	swfile->position = source->position;
	hash_add_swfile(swfile);

	return swfile;
}

gpsm_swfile_t *gpsm_swfile_link(gpsm_swfile_t *source)
{
	gpsm_swfile_t *swfile;

	if (!source)
		return NULL;

	swfile = (gpsm_swfile_t *)gpsm_newitem(GPSM_ITEM_TYPE_SWFILE);
	swfile->item.label = strdup(source->item.label);
	swfile->item.hposition = 0;
	swfile->item.vposition = 0;
	swfile->item.hsize = source->item.hsize;
	swfile->item.vsize = source->item.vsize;
	swfile->filename = source->filename;
	swfile->samplerate = source->samplerate;
	swfile->position = source->position;
	hash_add_swfile(swfile);

	return swfile;
}


gpsm_grp_t *gpsm_newgrp(const char *label)
{
	gpsm_grp_t *group;

	if (!label)
		return NULL;

	group = (gpsm_grp_t *)gpsm_newitem(GPSM_ITEM_TYPE_GRP);
	group->item.label = strdup(label);

	return group;
}


void gpsm_item_destroy(gpsm_item_t *item)
{
	if (!item)
		return;

	/* First make item unreachable. */
	gpsm_item_remove(item);
	if (GPSM_ITEM_IS_SWFILE(item))
		hash_remove_swfile((gpsm_swfile_t *)item);

	/* Send out the GPSM_SIG_ITEM_DESTROY signal. */
	glsig_emit(&item->emitter, GPSM_SIG_ITEM_DESTROY, item);

	/* Now delete based on type. */
	if (GPSM_ITEM_IS_SWFILE(item)) {
		gpsm_swfile_t *swfile = (gpsm_swfile_t *)item;
		/* We may delete unreachable swapfiles. */
		if (!hash_find_swfile(swfile->filename))
			sw_unlink(swfile->filename);
	} else if (GPSM_ITEM_IS_GRP(item)) {
		gpsm_grp_t *group = (gpsm_grp_t *)item;
		struct list_head *tmp;
		gpsm_item_t *i;
		/* Recursively delete all childs. */
		list_safe_foreach(&group->items, gpsm_item_t, list, tmp, i)
			gpsm_item_destroy(i);
	}

	/* Cleanup common parts and free the item. */
	if (item->label)
		free(item->label);
	glsig_delete_all(&item->emitter);
	free(item);
}



/*
 * Group insertion / removal API.
 */

static int _add_item_boundingbox(gpsm_grp_t *group, gpsm_item_t *item)
{
	int changed = 0;

	/* Fixup group boundingbox wrt item. */
	if (item->hposition + item->hsize > group->item.hsize) {
		group->item.hsize = item->hposition + item->hsize;
		changed = 1;
	}
	if (item->vposition + item->vsize > group->item.vsize) {
		group->item.vsize = item->vposition + item->vsize;
		changed = 1;
	}

	return changed;
}
static void gpsm_grp_fixup_boundingbox(gpsm_grp_t *group, gpsm_item_t *item)
{
	long hsize, vsize;

	/* Send out the GPSM_SIG_ITEM_CHANGED signal, if necessary.
	 * This will go up the tree and fix parents boundingboxes. */
	if (_add_item_boundingbox(group, item))
		glsig_emit(&group->item.emitter, GPSM_SIG_ITEM_CHANGED, group);

	/* If not, we still need to check for too large bb. */
	hsize = group->item.hsize;
	vsize = group->item.vsize;
	group->item.hsize = 0;
	group->item.vsize = 0;
	list_foreach(&group->items, gpsm_item_t, list, item)
		_add_item_boundingbox(group, item);
	if (hsize != group->item.hsize || vsize != group->item.vsize)
		glsig_emit(&group->item.emitter, GPSM_SIG_ITEM_CHANGED, group);
}
static void gpsm_grp_remove_boundingbox(gpsm_grp_t *group, gpsm_item_t *ritem)
{
	long hsize, vsize;
	gpsm_item_t *item;

	/* We have to check for too large bb. */
	hsize = group->item.hsize;
	vsize = group->item.vsize;
	group->item.hsize = 0;
	group->item.vsize = 0;
	list_foreach(&group->items, gpsm_item_t, list, item)
		if (item != ritem)
			_add_item_boundingbox(group, item);
	if (hsize != group->item.hsize || vsize != group->item.vsize)
		glsig_emit(&group->item.emitter, GPSM_SIG_ITEM_CHANGED, group);
}
static void handle_itemchange(glsig_handler_t *handler, long sig, va_list va)
{
	switch (sig) {
	case GPSM_SIG_ITEM_CHANGED: {
		gpsm_item_t *item;

		GLSIGH_GETARGS1(va, item);
		DPRINTF("got GPSM_SIG_ITEM_CHANGED from %s\n", item->label);

		/* Are we no longer member of a group? Can't be... */
		if (!item->parent)
			PANIC("Removed item with active signal handler.");

		/* Fixup boundingbox of our parent. */
		gpsm_grp_fixup_boundingbox(item->parent, item);

		break;
	}
	case GPSM_SIG_ITEM_REMOVE: {
		gpsm_item_t *item;
		gpsm_grp_t *grp;

		GLSIGH_GETARGS1(va, item);
		DPRINTF("got GPSM_SIG_ITEM_REMOVE from %s\n", item->label);
		grp = item->parent;

		/* Rebuild groups boundingbox. */
		gpsm_grp_remove_boundingbox(grp, item);

		/* Remove this signal handler, we will no longer need it. */
		glsig_delete_handler(handler);

		/* We dont need to send a signal out here, this will
		 * be done in gpsm_item_remove() anyway.
		 */

		break;
	}
	default:
		;
	}
}

int gpsm_grp_insert(gpsm_grp_t *group, gpsm_item_t *item,
		    long hposition, long vposition)
{
	if (!group || !item
	    || !list_empty(&item->list) || item->parent)
		return -1;

	/* FIXME - check overlap, sort items. */

	/* Fixup item position, handle magic -1 values. */
	if (hposition == -1 && vposition == -1)
		hposition = 0;
	if (hposition == -1)
		hposition = group->item.hsize;
	if (vposition == -1)
		vposition = group->item.vsize;
	item->hposition = hposition;
	item->vposition = vposition;

	/* Register an item changed signal handler to the new item
	 * to update the parents boundingbox, if necessary. */
	glsig_add_handler(&item->emitter, GPSM_SIG_ITEM_CHANGED|GPSM_SIG_ITEM_REMOVE,
			  handle_itemchange, item);

	/* Do the addition, send out GPSM_SIG_ITEM_CHANGED signal. */
	item->parent = group;
	list_add_tail(&item->list, &group->items);
	glsig_emit(&item->emitter, GPSM_SIG_ITEM_CHANGED, item);

	/* Send out GPSM_SIG_GRP_NEWITEM signal. */
	glsig_emit(&group->item.emitter, GPSM_SIG_GRP_NEWITEM, group, item);

	return 0;
}

void gpsm_item_remove(gpsm_item_t *item)
{
	gpsm_grp_t *grp;

	if (!item || list_empty(&item->list) || !(grp = item->parent))
		return;

	/* First send out GPSM_SIG_GRP_REMOVEITEM signal. This will
	 * fix grps boundingbox, too. */
	glsig_emit(&item->emitter, GPSM_SIG_ITEM_REMOVE, item);
	glsig_emit(&grp->item.emitter, GPSM_SIG_GRP_REMOVEITEM, grp, item);

	/* Do the removal. */
	list_del(&item->list);
	item->parent = NULL;
	item->hposition = 0;
	item->vposition = 0;

	/* Send out the GPSM_SIG_ITEM_CHANGED signal for both group
	 * and item. This will fix all other bounding boxes. */
	glsig_emit(&grp->item.emitter, GPSM_SIG_ITEM_CHANGED, grp);
	glsig_emit(&item->emitter, GPSM_SIG_ITEM_CHANGED, item);
}



/*
 * Item change API.
 */

void gpsm_item_set_label(gpsm_item_t *item, const char *label)
{
	if (!item || !label)
		return;
	free(item->label);
	item->label = strdup(label);
	glsig_emit(&item->emitter, GPSM_SIG_ITEM_CHANGED, item);
}

void gpsm_swfile_set(gpsm_swfile_t *swfile, int samplerate,
		     float position)
{
	if (!swfile)
		return;
	swfile->samplerate = samplerate;
	swfile->position = position;
	glsig_emit(gpsm_item_emitter(swfile), GPSM_SIG_ITEM_CHANGED, swfile);
}

void gpsm_swfile_set_samplerate(gpsm_swfile_t *swfile, int samplerate)
{
	if (!swfile)
		return;
	swfile->samplerate = samplerate;
	glsig_emit(gpsm_item_emitter(swfile), GPSM_SIG_ITEM_CHANGED, swfile);
}

void gpsm_swfile_set_position(gpsm_swfile_t *swfile, float position)
{
	if (!swfile)
		return;
	swfile->position = position;
	glsig_emit(gpsm_item_emitter(swfile), GPSM_SIG_ITEM_CHANGED, swfile);
}



/*
 * Connection with lowlevel swapfile API.
 */

void gpsm_notify_swapfile_change(long filename, long pos, long size)
{
	gpsm_swfile_t *swfile;
#ifdef DEBUG
	swfd_t fd;
	struct sw_stat st;
	long file_size;

	if ((fd = sw_open(filename, O_RDONLY, TXN_NONE)) == -1) {
		DPRINTF("Invalid filename\n");
		return;
	}
	sw_fstat(fd, &st);
	file_size = st.size/SAMPLE_SIZE;
	sw_close(fd);
#endif

	/* Loop over all references sending out the changed signal. */
	while ((swfile = hash_find_next_swfile(filename, swfile))) {
		glsig_emit(gpsm_item_emitter(swfile), GPSM_SIG_SWFILE_CHANGED,
			   swfile, pos, size);
#ifdef DEBUG
		if (gpsm_item_hsize(swfile) != file_size)
			DPRINTF("WARNING! Unnoticed swapfile change!\n");
#endif
	}
}

void gpsm_notify_swapfile_cut(long filename, long pos, long size)
{
	gpsm_swfile_t *swfile = NULL;
#ifdef DEBUG
	swfd_t fd;
	struct sw_stat st;
	long file_size;

	if ((fd = sw_open(filename, O_RDONLY, TXN_NONE)) == -1) {
		DPRINTF("Invalid filename\n");
		return;
	}
	sw_fstat(fd, &st);
	file_size = st.size/SAMPLE_SIZE;
	sw_close(fd);
#endif

	/* Loop over all references fixing sizes and sending out
	 * the appropriate signals. */
	while ((swfile = hash_find_next_swfile(filename, swfile))) {
		glsig_emit(gpsm_item_emitter(swfile),
			   GPSM_SIG_SWFILE_CUT, swfile, pos, size);
		swfile->item.hsize -= size;
		glsig_emit(gpsm_item_emitter(swfile),
			   GPSM_SIG_ITEM_CHANGED, swfile);
#ifdef DEBUG
		if (gpsm_item_hsize(swfile) != file_size)
			DPRINTF("WARNING! Unnoticed swapfile change!\n");
#endif
	}
}

void gpsm_notify_swapfile_insert(long filename, long pos, long size)
{
	gpsm_swfile_t *swfile = NULL;
#ifdef DEBUG
	swfd_t fd;
	struct sw_stat st;
	long file_size;

	if ((fd = sw_open(filename, O_RDONLY, TXN_NONE)) == -1) {
		DPRINTF("Invalid filename\n");
		return;
	}
	sw_fstat(fd, &st);
	file_size = st.size/SAMPLE_SIZE;
	sw_close(fd);
#endif

	/* Loop over all references fixing sizes and sending out
	 * the appropriate signals. */
	while ((swfile = hash_find_next_swfile(filename, swfile))) {
		glsig_emit(gpsm_item_emitter(swfile),
			   GPSM_SIG_SWFILE_INSERT, swfile, pos, size);
		swfile->item.hsize += size;
		glsig_emit(gpsm_item_emitter(swfile),
			   GPSM_SIG_ITEM_CHANGED, swfile);
#ifdef DEBUG
		if (gpsm_item_hsize(swfile) != file_size)
			DPRINTF("WARNING! Unnoticed swapfile change!\n");
#endif
	}
}

void gpsm_invalidate_swapfile(long filename)
{
	gpsm_swfile_t *swfile = NULL;
	swfd_t fd;
	struct sw_stat st;
	long old_size, new_size;

	/* Stat the file. */
	if ((fd = sw_open(filename, O_RDONLY, TXN_NONE)) == -1)
		return;
	sw_fstat(fd, &st);
	new_size = st.size/SAMPLE_SIZE;
	sw_close(fd);

	/* Loop over all references fixing sizes and sending out
	 * the appropriate signals. */
	while ((swfile = hash_find_next_swfile(filename, swfile))) {
		old_size = gpsm_item_hsize(swfile);

		/* First fix size by extending/cutting. */
		if (old_size < st.size/SAMPLE_SIZE) {
			glsig_emit(gpsm_item_emitter(swfile),
				   GPSM_SIG_SWFILE_INSERT, swfile, old_size,
				   new_size - old_size);
			swfile->item.hsize = new_size;
			glsig_emit(gpsm_item_emitter(swfile),
				   GPSM_SIG_ITEM_CHANGED, swfile);
		} else if (old_size > st.size/SAMPLE_SIZE) {
			glsig_emit(gpsm_item_emitter(swfile),
				   GPSM_SIG_SWFILE_CUT, swfile,
				   new_size, old_size - new_size);
			swfile->item.hsize = new_size;
			glsig_emit(gpsm_item_emitter(swfile),
				   GPSM_SIG_ITEM_CHANGED, swfile);
		}

		/* Second invalidate old data. */
		glsig_emit(gpsm_item_emitter(swfile), GPSM_SIG_SWFILE_CHANGED,
			   swfile, 0, MIN(old_size, new_size));
	}
}



/*
 * Tree access API.
 */

gpsm_grp_t *gpsm_root(void)
{
	return root;
}

gpsm_grp_t *gpsm_find_grp_label(gpsm_grp_t *root, gpsm_item_t *start,
				const char *label)
{
	gpsm_item_t *item, *next;

	if (!start) {
		item = list_gethead(&root->items, gpsm_item_t, list);
	} else {
		item = start;
		goto next_entry;
	}

	while (item) {
		if (GPSM_ITEM_IS_GRP(item)
		    && strcmp(item->label, label) == 0)
			return (gpsm_grp_t *)item;
next_entry:
		if (GPSM_ITEM_IS_GRP(item))
			next = list_gethead(&((gpsm_grp_t *)item)->items,
					    gpsm_item_t, list);
		else
			next = list_getnext(&item->parent->items, item,
					    gpsm_item_t, list);
		while (!next) {
			item = (gpsm_item_t *)item->parent;
			if (item == (gpsm_item_t *)root)
				return NULL;
			next = list_getnext(&item->parent->items, item,
					    gpsm_item_t, list);
		}
		item = next;
	}

	return NULL;
}

gpsm_swfile_t *gpsm_find_swfile_label(gpsm_grp_t *root, gpsm_item_t *start,
				      const char *label)
{
	gpsm_item_t *item, *next;

	if (!start) {
		item = list_gethead(&root->items, gpsm_item_t, list);
	} else {
		item = start;
		goto next_entry;
	}

	while (item) {
		if (GPSM_ITEM_IS_SWFILE(item)
		    && strcmp(item->label, label) == 0)
			return (gpsm_swfile_t *)item;
next_entry:
		if (GPSM_ITEM_IS_GRP(item))
			next = list_gethead(&((gpsm_grp_t *)item)->items,
					    gpsm_item_t, list);
		else
			next = list_getnext(&item->parent->items, item,
					    gpsm_item_t, list);
		while (!next) {
			item = (gpsm_item_t *)item->parent;
			if (item == (gpsm_item_t *)root)
				return NULL;
			next = list_getnext(&item->parent->items, item,
					    gpsm_item_t, list);
		}
		item = next;
	}

	return NULL;
}


gpsm_swfile_t *gpsm_find_swfile_filename(gpsm_grp_t *root, gpsm_item_t *start,
					 long filename)
{
	gpsm_item_t *item, *next;

	if (!start) {
		item = list_gethead(&root->items, gpsm_item_t, list);
	} else {
		item = start;
		goto next_entry;
	}

	while (item) {
		if (GPSM_ITEM_IS_SWFILE(item)
		    && gpsm_swfile_filename(item) == filename)
			return (gpsm_swfile_t *)item;
next_entry:
		if (GPSM_ITEM_IS_GRP(item))
			next = list_gethead(&((gpsm_grp_t *)item)->items,
					    gpsm_item_t, list);
		else
			next = list_getnext(&item->parent->items, item,
					    gpsm_item_t, list);
		while (!next) {
			item = (gpsm_item_t *)item->parent;
			if (item == (gpsm_item_t *)root)
				return NULL;
			next = list_getnext(&item->parent->items, item,
					    gpsm_item_t, list);
		}
		item = next;
	}

	return NULL;
}
