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


static gpsm_grp_t *root = NULL;

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
		xmlSetProp(child, "label", item->label);
		if (!list_empty(&group->items))
			dump_tree(group, child);
	} else if (GPSM_ITEM_IS_SWFILE(item)) {
		gpsm_swfile_t *swfile = (gpsm_swfile_t *)item;
		child = xmlNewChild(node, NULL, "file", NULL);
		xmlSetProp(child, "label", item->label);
		snprintf(s, 255, "%li", swfile->filename);
		xmlSetProp(child, "fd", s);
		snprintf(s, 255, "%i", swfile->samplerate);
		xmlSetProp(child, "rate", s);
		snprintf(s, 255, "%.3f", swfile->position);
		xmlSetProp(child, "position", s);
	}
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
        node = node->xmlChildrenNode;
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
		long ifd, irate;
		float iposition;
		struct sw_stat st;
		int fd;

		/* Extract file information. */
		if (!(c = xmlGetProp(node, "fd")))
			c = "-1";
		ifd = atoi(c);
		if (!(c = xmlGetProp(node, "rate")))
			c = "44100";
		irate = atoi(c);
		if (!(c = xmlGetProp(node, "position")))
			c = "0.0";
		irate = atof(c);

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
	ihposition = atoi(c);
	if (!(c = xmlGetProp(node, "vpos")))
		c = "0";
	ivposition = atoi(c);

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
	if (!(grp = gpsm_find_group_label(gpsm_root(), "unknown"))) {
		grp = gpsm_newgrp("unknown");
		gpsm_grp_insert(gpsm_root(), (gpsm_item_t *)grp, 0, 100000);
	}
	dir = sw_opendir();
	while ((name = sw_readdir(dir)) != -1) {
		if (name == 0)
			continue;
		if ((swfile = gpsm_find_swfile_filename(gpsm_root(), name)))
			continue;
		fd = sw_open(name, O_RDONLY, TXN_NONE);
		sw_fstat(fd, &st);
		sw_close(fd);
		swfile = gpsm_newswfile("unnamed");
		swfile->filename = name;
		swfile->item.hsize = st.size/SAMPLE_SIZE;
		swfile->item.vsize = 1;

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
		xml = strdup("\
<?xml version=\"1.0\"?>
<swapfile>
</swapfile>");
	}

	/* Try to parse the xml string. */
	if (!(doc = xmlParseMemory(xml, strlen(xml)))) {
		free(xml);
		return -1;
	}

	/* Create the tree root group and recurse down the xml tree. */
        root = (gpsm_grp_t *)gpsm_newitem(GPSM_ITEM_TYPE_GRP);
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
	DPRINTF("%i bytes xml %s\n", size, xml);
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



gpsm_grp_t *gpsm_root(void)
{
	return root;
}


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
	item->hposition = -1;
	item->vposition = -1;
	item->hsize = -1;
	item->vsize = -1;
	if (GPSM_ITEM_IS_GRP(item))
		INIT_LIST_HEAD(&((gpsm_grp_t *)item)->items);

	return item;
}

gpsm_swfile_t *gpsm_newswfile(const char *label)
{
	gpsm_swfile_t *swfile;
	swfd_t fd;

	if (!label)
		return NULL;

	swfile = (gpsm_swfile_t *)gpsm_newitem(GPSM_ITEM_TYPE_SWFILE);
	while ((fd = sw_open((swfile->filename = rand()),
			     O_RDWR|O_CREAT|O_EXCL, TXN_NONE)) == -1)
		;
	sw_close(fd);
	swfile->samplerate = -1;
	swfile->position = 0.0;
	swfile->item.label = strdup(label);

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

	/* Send out the GPSM_SIG_ITEM_DESTROY signal. */
	glsig_emit(&item->emitter, GPSM_SIG_ITEM_DESTROY, item);

	/* Now delete based on type. */
	if (GPSM_ITEM_IS_SWFILE(item)) {
		gpsm_swfile_t *swfile = (gpsm_swfile_t *)item;
		/* We may delete unreachable swapfiles. */
		if (!gpsm_find_swfile_filename(gpsm_root(), swfile->filename))
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


gpsm_swfile_t *gpsm_swfile_cow(gpsm_swfile_t *swfile)
{
	/* FIXME */
	return NULL;
}

gpsm_swfile_t *gpsm_swfile_link(gpsm_swfile_t *swfile)
{
	/* FIXME */
	return NULL;
}


static int _fixup_boundingbox(gpsm_grp_t *group, gpsm_item_t *item)
{
	int changed = 0;

	/* Fixup group boundingbox wrt item. */
	if (item->hposition < group->item.hposition
	    || group->item.hposition == -1) {
		group->item.hsize += group->item.hposition - item->hposition;
		group->item.hposition = item->hposition;
		changed = 1;
	}
	if (item->vposition < group->item.vposition
	    || group->item.vposition == -1) {
		group->item.vsize += group->item.vposition - item->vposition;
		group->item.vposition = item->vposition;
		changed = 1;
	}
	if (item->hposition + item->hsize > group->item.hposition + group->item.hsize) {
		group->item.hsize = item->hposition + item->hsize - group->item.hposition;
		changed = 1;
	}
	if (item->vposition + item->vsize > group->item.vposition + group->item.vsize) {
		group->item.vsize = item->vposition + item->vsize - group->item.vposition;
		changed = 1;
	}

	return changed;
}
static void gpsm_grp_fixup_boundingbox(gpsm_grp_t *group, gpsm_item_t *item)
{
	int changed;

	if (!group || !item)
		return;

	changed = _fixup_boundingbox(group, item);

	/* Send out the GPSM_SIG_ITEM_CHANGED signal, if necessary. */
	if (changed)
		glsig_emit(&group->item.emitter, GPSM_SIG_ITEM_CHANGED, group);

	/* Go upward the tree. */
	gpsm_grp_fixup_boundingbox(group->item.parent, (gpsm_item_t *)group);
}

int gpsm_grp_insert(gpsm_grp_t *group, gpsm_item_t *item,
		    long hposition, long vposition)
{
	if (!group || !item
	    || !list_empty(&item->list) || item->parent)
		return -1;

	/* FIXME - check overlap, sort items. */

	/* Fixup item position. */
	item->hposition = hposition;
	item->vposition = vposition;

	/* Do the addition, send out GPSM_SIG_ITEM_CHANGED signal. */
	item->parent = group;
	list_add_tail(&item->list, &group->items);
	glsig_emit(&item->emitter, GPSM_SIG_ITEM_CHANGED, item);

	/* Fix the groups bounding box. */
	gpsm_grp_fixup_boundingbox(group, item);

	/* Finally send out GPSM_SIG_GRP_NEWITEM signal. */
	glsig_emit(&group->item.emitter, GPSM_SIG_GRP_NEWITEM, group, item);

	return 0;
}

void gpsm_item_remove(gpsm_item_t *item)
{
	gpsm_grp_t *grp;

	if (!item || list_empty(&item->list) || !(grp = item->parent))
		return;

	/* First send out GPSM_SIG_GRP_REMOVEITEM signal. */
	glsig_emit(&grp->item.emitter, GPSM_SIG_GRP_REMOVEITEM, grp, item);

	/* Do the removal. */
	list_del(&item->list);
	item->parent = NULL;

	/* Fix the groups bounding box. */
	grp->item.hposition = -1;
	grp->item.vposition = -1;
	grp->item.hsize = -1;
	grp->item.vsize = -1;
	list_foreach(&grp->items, gpsm_item_t, list, item)
		_fixup_boundingbox(grp, item);

	/* Send out the GPSM_SIG_ITEM_CHANGED signal. */
	glsig_emit(&grp->item.emitter, GPSM_SIG_ITEM_CHANGED, grp);
}

void gpsm_item_set_label(gpsm_item_t *item, const char *label)
{
	if (!item || !label)
		return;
	free(item->label);
	item->label = strdup(label);
	glsig_emit(&item->emitter, GPSM_SIG_ITEM_CHANGED, item);
}

gpsm_grp_t *gpsm_find_group_label(gpsm_grp_t *root, const char *label)
{
	gpsm_item_t *item;
	gpsm_grp_t *group;

	/* Breath first search. */
	list_foreach(&root->items, gpsm_item_t, list, item) {
		if (!GPSM_ITEM_IS_GRP(item))
			continue;
		if (strcmp(item->label, label) == 0)
			return (gpsm_grp_t *)item;
	}
	list_foreach(&root->items, gpsm_item_t, list, item) {
		if (!GPSM_ITEM_IS_GRP(item))
			continue;
		if ((group = gpsm_find_group_label((gpsm_grp_t *)item, label)))
			return group;
	}

	return NULL;
}

gpsm_swfile_t *gpsm_find_swfile_label(gpsm_grp_t *root, const char *label)
{
	gpsm_item_t *item;
	gpsm_swfile_t *swfile;

	/* Breath first search. */
	list_foreach(&root->items, gpsm_item_t, list, item) {
		if (!GPSM_ITEM_IS_SWFILE(item))
			continue;
		if (strcmp(item->label, label) == 0)
			return (gpsm_swfile_t *)item;
	}
	list_foreach(&root->items, gpsm_item_t, list, item) {
		if (!GPSM_ITEM_IS_GRP(item))
			continue;
		if ((swfile = gpsm_find_swfile_label((gpsm_grp_t *)item, label)))
			return swfile;
	}

	return NULL;
}

gpsm_swfile_t *gpsm_find_swfile_filename(gpsm_grp_t *root, long filename)
{
	gpsm_item_t *item;
	gpsm_swfile_t *swfile;

	/* Breath first search. */
	list_foreach(&root->items, gpsm_item_t, list, item) {
		if (!GPSM_ITEM_IS_SWFILE(item))
			continue;
		if (((gpsm_swfile_t *)item)->filename == filename)
			return (gpsm_swfile_t *)item;
	}
	list_foreach(&root->items, gpsm_item_t, list, item) {
		if (!GPSM_ITEM_IS_GRP(item))
			continue;
		if ((swfile = gpsm_find_swfile_filename((gpsm_grp_t *)item, filename)))
			return swfile;
	}

	return NULL;
}
