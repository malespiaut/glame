/*
 * gpsm.c
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

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <xmlmemory.h>
#include <parser.h>
#include "glame_types.h"
#include "swapfile.h"
#include "gpsm.h"
#include "hash.h"
#include "glconfig.h"



/* The gpsm root and the swapfile filename hash. */
static gpsm_grp_t *root = NULL;
HASH(swfile, gpsm_swfile_t, 8,
     (swfile->filename == filename),
     (filename),
     (swfile->filename),
     long filename)

/* The operations structs. */
struct pair {
	long file;
	long saved;
};
struct op {
	struct glame_list_head list;
	struct timeval time;
	int is_undo; /* 1 if from undo, 0 if from prepare */
	int nrpairs;
	struct pair pair[0];
};

/* List of ops, sorted by time, head is latest, configurables */
struct glame_list_head oplist;
static long op_max_listsize = 5;
static int op_listsize;


/* Forwards. */
static gpsm_item_t *gpsm_newitem(int type);
static int _gpsm_get_swfiles(gpsm_item_t *root, gpsm_swfile_t ***files);
static struct op *_op_new(int nrpairs);
static void _op_delete(struct op *op);
static struct op *_op_prepare(gpsm_item_t *item);
static int _op_cow(struct op *op);
static void _op_add(struct op *op);
static void _op_fix_swfiles(struct op *op);
static int _op_undo(struct op *op);
static struct op *_op_find_filename(long filename);
static struct op *_op_find_filename_before(long filename, struct op *op);
static struct op *_op_find_saved(long filename);
struct op *_op_get(gpsm_item_t *item);
static struct op *_op_kill_redo(struct op *op);
static struct op *_op_get_prev(struct op *op);
struct op *_op_get_redo(gpsm_swfile_t **files, int cnt);
struct op *_op_get_undo(gpsm_swfile_t **files, int cnt);

/* Macros for easier use. */
#define X0(item) (gpsm_item_hposition(item))
#define X1(item) (gpsm_item_hposition(item) + gpsm_item_hsize(item) - 1)
#define Y0(item) (gpsm_item_vposition(item))
#define Y1(item) (gpsm_item_vposition(item) + gpsm_item_vsize(item) - 1)
#define POS_IS_INSIDE(x, y, item) \
        (X0(item) <= x && x <= X1(item) && Y0(item) <= y && y <= Y1(item))
#define ITEMS_DO_OVERLAP(it1, it2) \
        (POS_IS_INSIDE(X0(it1), Y0(it1), it2) \
	 || POS_IS_INSIDE(X1(it1), Y0(it1), it2) \
	 || POS_IS_INSIDE(X0(it1), Y1(it1), it2) \
	 || POS_IS_INSIDE(X1(it1), Y1(it1), it2) \
         || POS_IS_INSIDE(X0(it2), Y0(it2), it1) \
	 || POS_IS_INSIDE(X1(it2), Y0(it2), it1) \
	 || POS_IS_INSIDE(X0(it2), Y1(it2), it1) \
	 || POS_IS_INSIDE(X1(it2), Y1(it2), it1))


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
		if (!glame_list_empty(&group->items))
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
	glame_list_foreach(&tree->items, gpsm_item_t, list, item)
		dump_item(item, node);
}
static void dump_ops(xmlNodePtr node)
{
	struct op *op;
	xmlNodePtr opnode, pairnode;
	char s[256];
	int i;

	/* Dump from back to front to get implicit ordering right. */
	op = glame_list_gettail(&oplist, struct op, list);
	while (op) {
		opnode = xmlNewChild(node, NULL, "op", NULL);
		snprintf(s, 255, "%i", op->nrpairs);
		xmlSetProp(opnode, "nrpairs", s);
		snprintf(s, 255, "%i", op->is_undo);
		xmlSetProp(opnode, "is_undo", s);
		for (i=0; i<op->nrpairs; i++) {
			pairnode = xmlNewChild(opnode, NULL, "pair", NULL);
			snprintf(s, 255, "%li", op->pair[i].file);
			xmlSetProp(pairnode, "file", s);
			snprintf(s, 255, "%li", op->pair[i].saved);
			xmlSetProp(pairnode, "saved", s);
		}
		op = glame_list_getprev(&oplist, op, struct op, list);
	}
}

static void insert_node_op(xmlNodePtr node);
static void insert_node_file(gpsm_grp_t *tree, xmlNodePtr node);
static void insert_node_grp(gpsm_grp_t *tree, xmlNodePtr node);
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
		if (strcmp(node->name, "group") == 0)
			insert_node_grp(tree, node);
		else if (strcmp(node->name, "file") == 0)
			insert_node_file(tree, node);
		else if (strcmp(node->name, "op") == 0)
			insert_node_op(node);
		else if (strcmp(node->name, "text") == 0)
			/* ignore */ ;
		else
			DPRINTF("Illegal node \"%s\" in xml\n", node->name);
		node = node->next;
	}
}
static void insert_node_file(gpsm_grp_t *tree, xmlNodePtr node)
{
	char *c, *ilabel;
	long ihposition, ivposition;
	gpsm_swfile_t *swfile;
	long ifd;
	float iposition;
	struct sw_stat st;
	int irate, fd;

	/* Extract file information out of the node. */
	if (!(c = xmlGetProp(node, "label")))
		c = "(unnamed)";
	ilabel = strdup(c);
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
	if (sscanf(c, "%f", &iposition) != 1)
		iposition = 0.0;
	if (!(c = xmlGetProp(node, "hpos")))
		c = "0";
	if (sscanf(c, "%li", &ihposition) != 1)
		ihposition = 0;
	if (!(c = xmlGetProp(node, "vpos")))
		c = "0";
	if (sscanf(c, "%li", &ivposition) != 1)
		ivposition = -1;

	/* Check, if the file is really there (and update info) */
	if ((fd = sw_open(ifd, O_RDONLY)) != -1) {
		sw_fstat(fd, &st);
		sw_close(fd);
	} else {
		DPRINTF("%s does not exist\n", ilabel);
		free(ilabel);
		return;
	}

	/* Create the swfile. */
	swfile = (gpsm_swfile_t *)gpsm_newitem(GPSM_ITEM_TYPE_SWFILE);
	swfile->item.label = ilabel;
	swfile->filename = ifd;
	swfile->samplerate = irate;
	swfile->position = iposition;
	swfile->item.hsize = st.size/SAMPLE_SIZE;
	swfile->item.vsize = 1;
	hash_add_swfile(swfile);
	if (gpsm_item_place(tree, (gpsm_item_t *)swfile, ihposition, ivposition) == -1)
		DPRINTF("WARNING: placement of %s at %li, %li rejected\n",
			gpsm_item_label(swfile), ihposition, ivposition);
}
static void insert_node_grp(gpsm_grp_t *tree, xmlNodePtr node)
{
        gpsm_item_t *item;
	char *c, *ilabel;
	long ihposition, ivposition;

	/* Extract label, hpos and vpos out of node. */
	if (!(c = xmlGetProp(node, "label")))
		c = "(unnamed)";
	ilabel = strdup(c);
	if (!(c = xmlGetProp(node, "hpos")))
		c = "0";
	if (sscanf(c, "%li", &ihposition) != 1)
		ihposition = 0;
	if (!(c = xmlGetProp(node, "vpos")))
		c = "0";
	if (sscanf(c, "%li", &ivposition) != 1)
		ivposition = -1;

	/* Create new group and insert it into tree. */
	item = gpsm_newitem(GPSM_ITEM_TYPE_GRP);
	item->label = ilabel;
	if (gpsm_item_place(tree, item, ihposition, ivposition) == -1)
		DPRINTF("WARNING: placement of %s at %li, %li rejected\n",
			gpsm_item_label(item), ihposition, ivposition);

	/* Recurse down the childrens. */
	insert_childs((gpsm_grp_t *)item, node);
}
static void insert_node_op(xmlNodePtr node)
{
	char *c;
	int icnt, iisundo, i;
	struct op *op;
	swfd_t fd;

	if (!(c = xmlGetProp(node, "nrpairs"))) {
		DPRINTF("Invalid <op>\n");
		return;
	}
	if (sscanf(c, "%i", &icnt) != 1) {
		DPRINTF("Invalid nrpairs %s\n", c);
		return;
	}
	if (!(c = xmlGetProp(node, "is_undo")))
		c = "0";
	if (sscanf(c, "%i", &iisundo) != 1) {
		DPRINTF("Invalid is_undo %s\n", c);
		return;
	}
	op = _op_new(icnt);
	op->is_undo = iisundo;
#ifndef xmlChildrenNode
	node = node->childs;
#else
	node = node->xmlChildrenNode;
#endif
	if (!node) {
		DPRINTF("Invalid <op> childs\n");
		return;
	}
	i=0;
	while (node) {
		if (strcmp(node->name, "text") == 0)
			goto next; /* Ignore */
		else if (strcmp(node->name, "pair") != 0) {
			DPRINTF("Invalid <op> child entry\n");
			return;
		}
		if (i == icnt) {
			DPRINTF("Too many pairs in op\n");
			break;
		}
		if (!(c = xmlGetProp(node, "file"))) {
			DPRINTF("Invalid <pair>\n");
			return;
		}
		if (sscanf(c, "%li", &op->pair[i].file) != 1) {
			DPRINTF("Invalid <pair> file %s\n", c);
			return;
		}
		if (!(c = xmlGetProp(node, "saved"))) {
			DPRINTF("Invalid <pair>\n");
			return;
		}
		if (sscanf(c, "%li", &op->pair[i].saved) != 1) {
			DPRINTF("Invalid <pair> saved %s\n", c);
			return;
		}
		if ((fd = sw_open(op->pair[i].file, O_RDONLY)) == -1)
			goto next;
		sw_close(fd);
		if ((fd = sw_open(op->pair[i].saved, O_RDONLY)) == -1)
			goto next;
		sw_close(fd);
		i++;
	next:
		node = node->next;
	}
	if (i < icnt) {
		DPRINTF("Too less pairs in op\n");
		op->nrpairs = i;
	}
	if (i == 0) {
		free(op);
		return;
	}
	_op_add(op);
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
	if (!(grp = gpsm_find_grp_label(gpsm_root(), NULL, GPSM_GRP_UNRECOGNIZED_LABEL))) {
		grp = gpsm_newgrp(GPSM_GRP_UNRECOGNIZED_LABEL);
		gpsm_item_place(gpsm_root(), (gpsm_item_t *)grp, 0, GPSM_GRP_UNRECOGNIZED_VPOS);
	}
	dir = sw_opendir();
	while ((name = sw_readdir(dir)) != -1) {
		if (name == 0)
			continue;
		/* File in tree? */
		if (hash_find_swfile(name))
			continue;
		/* File in op list as saved? */
		if (_op_find_saved(name))
			continue;
		fd = sw_open(name, O_RDONLY);
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
		gpsm_item_place(grp, (gpsm_item_t *)swfile,
				0, gpsm_item_vsize(grp));
	}
	sw_closedir(dir);

	/* Check if the unknown group is empty and if so, remove it. */
	if (glame_list_empty(&grp->items))
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

	/* Open the swapfile. */
	if (swapfile_open(swapfile, 0) == -1) {
		if (errno != EBUSY) {
			perror("ERROR: Unable to open swap");
			return -1;
		}
		fprintf(stderr, "WARNING: Unclean swap - running fsck\n");
		if (swapfile_fsck(swapfile, 0) == -1) {
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
	if ((fd = sw_open(0, O_RDONLY)) != -1) {
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
empty:
		/* Seems to be empty swapfile - use "default" xml. */
		xml = strdup(""
"<?xml version=\"1.0\"?>\n"
"<swapfile/>\n");
	}

	/* Try to parse the xml string. */
	if (!(doc = xmlParseMemory(xml, strlen(xml)))) {
		/* Doh. Failed. Startup with empty meta. */
		free(xml);
		goto empty;
	}

	/* Init the op list. */
	GLAME_INIT_LIST_HEAD(&oplist);
	op_listsize = 0;
	glame_config_get_long("swapfile/maxundo", &op_max_listsize);

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

int gpsm_set_max_saved_ops(int max)
{
	struct op *op;

	if (max >= 0)
		op_max_listsize = max;
	if (root)
		while (op_listsize > op_max_listsize) {
			DPRINTF("shrinking undo stack\n");
			op = glame_list_gettail(&oplist, struct op, list);
			_op_delete(op);
		}
	return op_max_listsize;
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
	dump_ops(docroot);
	xmlDocSetRootElement(doc, docroot);
	xmlDocDumpMemory(doc, &xml, &size);
	DPRINTF("%i bytes xml\n %s\n", size, xml);
	fd = sw_open(0, O_RDWR|O_CREAT|O_TRUNC);
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
	GLAME_INIT_LIST_HEAD(&item->list);
	item->parent = NULL;
	INIT_GLSIG_EMITTER(&item->emitter);
	item->type = type;
	item->label = NULL;
	item->hposition = 0;
	item->vposition = 0;
	item->hsize = 0;
	item->vsize = 0;
	if (GPSM_ITEM_IS_GRP(item))
		GLAME_INIT_LIST_HEAD(&((gpsm_grp_t *)item)->items);
	else if (GPSM_ITEM_IS_SWFILE(item)) {
		hash_init_swfile((gpsm_swfile_t *)item);
		item->hsize = 0;
		item->vsize = 1;
		((gpsm_swfile_t *)item)->filename = -1;
		((gpsm_swfile_t *)item)->last_op_time.tv_sec = 0;
		((gpsm_swfile_t *)item)->last_op_time.tv_usec = 0;
	}

	return item;
}


gpsm_swfile_t *gpsm_newswfile(const char *label)
{
	gpsm_swfile_t *swfile;
	swfd_t fd;

	if (!root || !label)
		return NULL;

	swfile = (gpsm_swfile_t *)gpsm_newitem(GPSM_ITEM_TYPE_SWFILE);
	swfile->item.label = strdup(label);
	swfile->item.hposition = 0;
	swfile->item.vposition = 0;
	swfile->item.hsize = 0;
	swfile->item.vsize = 1;
	while ((fd = sw_open((swfile->filename = rand()),
			     O_RDWR|O_CREAT|O_EXCL)) == -1)
		;
	sw_close(fd);
	swfile->samplerate = glame_config_get_long_with_default("audio_io/input_rate", 
								GLAME_DEFAULT_SAMPLERATE);
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

	if (!root || !source || !GPSM_ITEM_IS_SWFILE(source))
		return NULL;

	if ((sfd = sw_open(source->filename, O_RDONLY)) == -1)
		return NULL;
	if (sw_fstat(sfd, &st) == -1) {
		sw_close(sfd);
		return NULL;
	}

	swfile = (gpsm_swfile_t *)gpsm_newitem(GPSM_ITEM_TYPE_SWFILE);
	while ((dfd = sw_open((swfile->filename = rand()),
			      O_RDWR|O_CREAT|O_EXCL)) == -1)
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

	if (!root || !source || !GPSM_ITEM_IS_SWFILE(source))
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

	if (!root || !label)
		return NULL;

	group = (gpsm_grp_t *)gpsm_newitem(GPSM_ITEM_TYPE_GRP);
	group->item.label = strdup(label);

	return group;
}

gpsm_grp_t *gpsm_grp_cow(gpsm_grp_t *grp)
{
	gpsm_grp_t *copy;
	gpsm_item_t *item;

	if (!root || !grp)
		return NULL;

	copy = gpsm_newgrp(gpsm_item_label(grp));
	gpsm_grp_foreach_item(grp, item) {
		gpsm_item_t *c;
		if (GPSM_ITEM_IS_SWFILE(item))
			c = (gpsm_item_t *)gpsm_swfile_cow((gpsm_swfile_t *)item);
		else if (GPSM_ITEM_IS_GRP(item))
			c = (gpsm_item_t *)gpsm_grp_cow((gpsm_grp_t *)item);
		else
			continue;
		gpsm_item_place(copy, c,
				gpsm_item_hposition(item), gpsm_item_vposition(item));
	}

	return copy;
}

gpsm_grp_t *gpsm_grp_link(gpsm_grp_t *grp)
{
	gpsm_grp_t *copy;
	gpsm_item_t *item;

	if (!root || !grp)
		return NULL;

	copy = gpsm_newgrp(gpsm_item_label(grp));
	gpsm_grp_foreach_item(grp, item) {
		gpsm_item_t *c;
		if (GPSM_ITEM_IS_SWFILE(item))
			c = (gpsm_item_t *)gpsm_swfile_link((gpsm_swfile_t *)item);
		else if (GPSM_ITEM_IS_GRP(item))
			c = (gpsm_item_t *)gpsm_grp_link((gpsm_grp_t *)item);
		else
			continue;
		gpsm_item_place(copy, c,
				gpsm_item_hposition(item), gpsm_item_vposition(item));
	}

	return copy;
}



void gpsm_item_destroy(gpsm_item_t *item)
{
	if (!root || !item)
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
		if (!hash_find_swfile(swfile->filename)) {
			/* Kill off pending ops on this file. */
			struct op *op = _op_find_filename(swfile->filename);
			int i;
			while (op) {
				for (i=0; i<op->nrpairs; i++) {
					while (op->pair[i].file == swfile->filename) {
						sw_unlink(op->pair[i].saved);
						op->pair[i].saved = -1;
						op->nrpairs--;
						if (i == op->nrpairs)
							break;
						op->pair[i] = op->pair[op->nrpairs];
					}
				}
				if (op->nrpairs == 0) {
					_op_delete(op);
					op = (struct op *)&oplist;
				}
				op = _op_find_filename_before(swfile->filename, op);
			}
			sw_unlink(swfile->filename);
		}
	} else if (GPSM_ITEM_IS_GRP(item)) {
		gpsm_grp_t *group = (gpsm_grp_t *)item;
		struct glame_list_head *tmp;
		gpsm_item_t *i;
		/* Recursively delete all childs. */
		glame_list_safe_foreach(&group->items, gpsm_item_t, list, tmp, i)
			gpsm_item_destroy(i);
	}

	/* Cleanup common parts and free the item. */
	if (item->label)
		free(item->label);
	glsig_delete_all(&item->emitter);
	free(item);
}

int gpsm_grp_is_hbox(gpsm_grp_t *group)
{
	gpsm_item_t *item;
	long end = -1;

	if (!root || !group || !GPSM_ITEM_IS_GRP(group))
		return 0;

	/* hbox means, we ignore vposition/vsize completely and just
	 * have all group items sorted by their hposition. It also
	 * means we do not allow overlapping of the "horizontal intervals"
	 * of the items (regardless, of real overlap). */
	gpsm_grp_foreach_item(group, item) {
		if (gpsm_item_hposition(item) <= end)
			return 0;
		end = gpsm_item_hposition(item) + gpsm_item_hsize(item) - 1;
	}
	return 1;
}

int gpsm_grp_is_vbox(gpsm_grp_t *group)
{
	gpsm_item_t *item;
	long end = -1;

	if (!root || !group || !GPSM_ITEM_IS_GRP(group))
		return 0;

	/* vbox means, we ignore hposition/hsize completely and just
	 * have all group items sorted by their vposition. It also
	 * means we do not allow overlapping of the "vertical intervals"
	 * of the items (regardless, of real overlap). */
	gpsm_grp_foreach_item(group, item) {
		if (gpsm_item_vposition(item) <= end)
			return 0;
		end = gpsm_item_vposition(item) + gpsm_item_vsize(item) - 1;
	}
	return 1;
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
	glame_list_foreach(&group->items, gpsm_item_t, list, item)
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
	glame_list_foreach(&group->items, gpsm_item_t, list, item)
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

static void _place_tail(gpsm_grp_t *grp,
			gpsm_item_t *item, gpsm_item_t *succ,
			long hpos, long vpos)
{
	/* Do the addition, send out appropriate signals. */
	item->parent = grp;
	item->hposition = hpos;
	item->vposition = vpos;
	if (!succ)
		glame_list_add_tail(&item->list, &grp->items);
	else
		glame_list_add_tail(&item->list, &succ->list);
	glsig_emit(&item->emitter, GPSM_SIG_ITEM_CHANGED, item);

	/* Send out GPSM_SIG_GRP_NEWITEM signal. */
	glsig_emit(&grp->item.emitter, GPSM_SIG_GRP_NEWITEM, grp, item);
}

int gpsm_item_can_place(gpsm_grp_t *grp, gpsm_item_t *item,
			long hpos, long vpos)
{
	gpsm_item_t *it, dummy;
	int can;

	if (!root || !grp || !GPSM_ITEM_IS_GRP(grp)
	    || !item || (gpsm_item_t *)grp == item
	    || hpos < 0 || vpos < 0)
		return 0;

	/* Check, if the box (hpos, vpos) - (hpos+hsize, vpos+vsize)
	 * overlaps with a box inside grp _or_ the placement will
	 * cause the grp to expand and overlap with something along
	 * the grps parents.
	 * Note that we need to ignore the item itself.
	 */

	/* FIXME: handle pre-removal (and grp fixup) of item from
	 *        its old position! */

	dummy.hposition = hpos;
	dummy.vposition = vpos;
	dummy.hsize = MAX(1, gpsm_item_hsize(item));
	dummy.vsize = MAX(1, gpsm_item_vsize(item));
	gpsm_grp_foreach_item(grp, it) {
		if (it == item)
			continue;
		if (ITEMS_DO_OVERLAP(it, &dummy))
			return 0;
	}

	/* Now we need to "recurse" upward, trying to replace the
	 * (possibly) enlarged grp at its old position. Note that
	 * we dont need to handle shrinking, as the larger box did
	 * fit already. Also we can bail out, if grp->parent == NULL.
	 */
	if (!gpsm_item_parent(grp)
	    || gpsm_item_hsize(grp) >= hpos + gpsm_item_hsize(item)
	    || gpsm_item_vsize(grp) >= vpos + gpsm_item_vsize(item))
		return 1;
	/* Save grps size - adjust it for recursion. */
	dummy.hsize = gpsm_item_hsize(grp);
	dummy.vsize = gpsm_item_vsize(grp);
	grp->item.hsize = MAX(gpsm_item_hsize(grp),
			      hpos + gpsm_item_hsize(item));
	grp->item.vsize = MAX(gpsm_item_vsize(grp),
			      vpos + gpsm_item_vsize(item));
	can = gpsm_item_can_place(gpsm_item_parent(grp), (gpsm_item_t *)grp,
				  gpsm_item_hposition(grp),
				  gpsm_item_vposition(grp));
	grp->item.hsize = dummy.hsize;
	grp->item.vsize = dummy.vsize;

	return can;
}

int gpsm_item_place(gpsm_grp_t *grp, gpsm_item_t *item,
		    long hpos, long vpos)
{
	gpsm_item_t *succ;

	if (!root || !gpsm_item_can_place(grp, item, hpos, vpos))
		return -1;

	/* Remove the item from its old position. */
	gpsm_item_remove(item);

	/* Register an item changed signal handler to the new item
	 * to update the parents boundingbox, if necessary. */
	glsig_add_handler(&item->emitter,
			  GPSM_SIG_ITEM_CHANGED|GPSM_SIG_ITEM_REMOVE,
			  handle_itemchange, item);

	/* Find the insertion point inside the grps child list.
	 * Try to preserve hbox / vbox properties. */
	if (gpsm_grp_is_vbox(grp) && !gpsm_grp_is_hbox(grp)) {
		gpsm_grp_foreach_item(grp, succ)
			if (Y0(succ) >= vpos)
				break;
	} else if (gpsm_grp_is_hbox(grp) && !gpsm_grp_is_vbox(grp)) {
		gpsm_grp_foreach_item(grp, succ)
			if (X0(succ) >= hpos)
				break;
	} else /* if its neither, try to be clever */ {
		gpsm_grp_foreach_item(grp, succ)
			if (Y0(succ) > vpos || X0(succ) > hpos)
				break;
	}

	/* Do the placement. */
	_place_tail(grp, item, succ, hpos, vpos);

	return 0;
}

int gpsm_hbox_can_insert(gpsm_grp_t *grp, gpsm_item_t *item,
			 long hpos, long vpos)
{
	gpsm_item_t *it;

	if (!root || !grp || !GPSM_ITEM_IS_GRP(grp)
	    || !item || (gpsm_item_t *)grp == item
	    || hpos < 0 || vpos < 0
	    || !gpsm_grp_is_hbox(grp))
		return 0;

	/* Can hbox-insert is very simple, we just need to check if
	 * hpos is inside an item of grp. Note that insertion to a
	 * start edge position of an existing item is allowed. */
	gpsm_grp_foreach_item(grp, it) {
		if (item == it)
			continue;
		if (X0(it) < hpos && hpos <= X1(it))
			return 0;
	}

	return 1;
}

int gpsm_vbox_can_insert(gpsm_grp_t *grp, gpsm_item_t *item,
			 long hpos, long vpos)
{
	gpsm_item_t *it;

	if (!root || !grp || !GPSM_ITEM_IS_GRP(grp)
	    || !item || (gpsm_item_t *)grp == item
	    || hpos < 0 || vpos < 0
	    || !gpsm_grp_is_vbox(grp))
		return 0;

	/* Can hbox-insert is very simple, we just need to check if
	 * hpos is inside an item of grp. Note that insertion to a
	 * start edge position of an existing item is allowed. */
	gpsm_grp_foreach_item(grp, it) {
		if (item == it)
			continue;
		if (Y0(it) < vpos && vpos <= Y1(it))
			return 0;
	}

	return 1;
}

/* Transparently handle item moving top-down / back-front. */
static void _gpsm_item_insert_space_after(gpsm_item_t *item, long dx, long dy)
{
	gpsm_item_t *it;

	/* Top - down */
	if (!gpsm_item_parent(item))
		return;
	_gpsm_item_insert_space_after((gpsm_item_t *)gpsm_item_parent(item),
				      dx, dy);

	/* Back to front. */
	it = glame_list_gettail(&gpsm_item_parent(item)->items, gpsm_item_t, list);
	while (it && it != item) {
		it->hposition += dx;
		it->vposition += dy;
		glsig_emit(gpsm_item_emitter(it), GPSM_SIG_ITEM_CHANGED, it);
		it = glame_list_getprev(&gpsm_item_parent(item)->items, it,
				  gpsm_item_t, list);
	}
}
static void _gpsm_item_insert_space_before(gpsm_item_t *item, long dx, long dy)
{
	gpsm_item_t *it;

	/* Top - down */
	if (!gpsm_item_parent(item))
		return;
	_gpsm_item_insert_space_after((gpsm_item_t *)gpsm_item_parent(item),
				      dx, dy);

	/* Back to front. */
	it = glame_list_gettail(&gpsm_item_parent(item)->items, gpsm_item_t, list);
	do {
		it->hposition += dx;
		it->vposition += dy;
		glsig_emit(gpsm_item_emitter(it), GPSM_SIG_ITEM_CHANGED, it);
		if (it == item)
			break;
		it = glame_list_getprev(&gpsm_item_parent(item)->items, it,
				  gpsm_item_t, list);
	} while (it);
}

int gpsm_hbox_insert(gpsm_grp_t *hbox, gpsm_item_t *item,
		     long hposition, long vposition)
{
	gpsm_item_t *succ;

	if (!root || !gpsm_hbox_can_insert(hbox, item, hposition, vposition))
		return -1;

	/* Cut out the item first - closing the gap it leaves.
	 * FIXME: whats the removal semantic we want to have? */
	if (gpsm_grp_is_hbox(gpsm_item_parent(item)))
		gpsm_hbox_cut(item);
	else if (gpsm_grp_is_vbox(gpsm_item_parent(item)))
		gpsm_vbox_cut(item);
	else {
		DPRINTF("WARNING: doing ordinary remove on hbox_insert\n");
		gpsm_item_remove(item);
	}

	/* Register an item changed signal handler to the new item
	 * to update the parents boundingbox, if necessary. */
	glsig_add_handler(&item->emitter,
			  GPSM_SIG_ITEM_CHANGED|GPSM_SIG_ITEM_REMOVE,
			  handle_itemchange, item);

	/* Find insertion point - succ will point to the successor. */
	gpsm_grp_foreach_item(hbox, succ) {
		if (gpsm_item_hposition(succ) >= hposition)
			break;
	}

	/* Move all nodes beginning at succ by item hsize. */
	if (succ)
		_gpsm_item_insert_space_before(succ, gpsm_item_hsize(item), 0);
	else
		_gpsm_item_insert_space_after((gpsm_item_t *)hbox,
					      gpsm_item_hsize(item), 0);

	/* Do the insertion. */
	_place_tail(hbox, item, succ, hposition, vposition);

	return 0;
}

int gpsm_vbox_insert(gpsm_grp_t *vbox, gpsm_item_t *item,
		     long hposition, long vposition)
{
	gpsm_item_t *succ;

	if (!root || !gpsm_vbox_can_insert(vbox, item, hposition, vposition))
		return -1;

	/* Cut out the item first - closing the gap it leaves.
	 * FIXME: whats the removal semantic we want to have? */
	if (gpsm_grp_is_vbox(gpsm_item_parent(item)))
		gpsm_vbox_cut(item);
	else if (gpsm_grp_is_hbox(gpsm_item_parent(item)))
		gpsm_hbox_cut(item);
	else {
		DPRINTF("WARNING: doing ordinary remove on vbox_insert\n");
		gpsm_item_remove(item);
	}

	/* Register an item changed signal handler to the new item
	 * to update the parents boundingbox, if necessary. */
	glsig_add_handler(&item->emitter,
			  GPSM_SIG_ITEM_CHANGED|GPSM_SIG_ITEM_REMOVE,
			  handle_itemchange, item);

	/* Find insertion point - succ will point to the successor. */
	gpsm_grp_foreach_item(vbox, succ) {
		if (gpsm_item_vposition(succ) >= vposition)
			break;
	}

	/* Move all nodes beginning at succ by item vsize. */
	if (succ)
		_gpsm_item_insert_space_before(succ, 0, gpsm_item_vsize(item));
	else
		_gpsm_item_insert_space_after((gpsm_item_t *)vbox,
					      0, gpsm_item_vsize(item));

	/* Do the insertion. */
	_place_tail(vbox, item, succ, hposition, vposition);

	return 0;
}

void gpsm_item_remove(gpsm_item_t *item)
{
	gpsm_grp_t *grp;

	if (!root || !item || glame_list_empty(&item->list) || !(grp = item->parent))
		return;

	/* First send out GPSM_SIG_GRP_REMOVEITEM signal. This will
	 * fix grps boundingbox, too. */
	glsig_emit(&item->emitter, GPSM_SIG_ITEM_REMOVE, item);
	glsig_emit(&grp->item.emitter, GPSM_SIG_GRP_REMOVEITEM, grp, item);

	/* Do the removal. */
	glame_list_del_init(&item->list);
	item->parent = NULL;
	item->hposition = 0;
	item->vposition = 0;

	/* Send out the GPSM_SIG_ITEM_CHANGED signal for both group
	 * and item. This will fix all other bounding boxes. */
	glsig_emit(&grp->item.emitter, GPSM_SIG_ITEM_CHANGED, grp);
	glsig_emit(&item->emitter, GPSM_SIG_ITEM_CHANGED, item);
}

int gpsm_hbox_cut(gpsm_item_t *item)
{
	gpsm_item_t *succ;
	gpsm_grp_t *parent;

	if (!root || !item || !(parent = gpsm_item_parent(item))
	    || !gpsm_grp_is_hbox(parent))
		return -1;

	/* First remove the item, remember successor */
	succ = glame_list_getnext(&parent->items, item, gpsm_item_t, list);
	gpsm_item_remove(item);

	/* Then close the gap it left. */
	if (succ)
		_gpsm_item_insert_space_before(succ, -gpsm_item_hsize(item), 0);
	else if (parent)
		_gpsm_item_insert_space_after((gpsm_item_t *)parent,
					      -gpsm_item_hsize(item), 0);

	return 0;
}

int gpsm_vbox_cut(gpsm_item_t *item)
{
	gpsm_item_t *succ;
	gpsm_grp_t *parent;

	if (!root || !item || !(parent = gpsm_item_parent(item))
	    || !gpsm_grp_is_vbox(parent))
		return -1;

	/* First remove the item, remember successor */
	succ = glame_list_getnext(&parent->items, item, gpsm_item_t, list);
	gpsm_item_remove(item);

	/* Then close the gap it left. */
	if (succ)
		_gpsm_item_insert_space_before(succ, 0, -gpsm_item_vsize(item));
	else if (parent)
		_gpsm_item_insert_space_after((gpsm_item_t *)parent,
					      0, -gpsm_item_vsize(item));

	return 0;
}




/*
 * Item change API.
 */

void gpsm_item_set_label(gpsm_item_t *item, const char *label)
{
	if (!root || !item || !label)
		return;
	free(item->label);
	item->label = strdup(label);
	glsig_emit(&item->emitter, GPSM_SIG_ITEM_CHANGED, item);
}

void gpsm_swfile_set(gpsm_swfile_t *swfile, int samplerate,
		     float position)
{
	if (!root || !swfile || !GPSM_ITEM_IS_SWFILE(swfile))
		return;
	swfile->samplerate = samplerate;
	swfile->position = position;
	glsig_emit(gpsm_item_emitter(swfile), GPSM_SIG_ITEM_CHANGED, swfile);
}

void gpsm_swfile_set_samplerate(gpsm_swfile_t *swfile, int samplerate)
{
	if (!root || !swfile || !GPSM_ITEM_IS_SWFILE(swfile))
		return;
	swfile->samplerate = samplerate;
	glsig_emit(gpsm_item_emitter(swfile), GPSM_SIG_ITEM_CHANGED, swfile);
}

void gpsm_swfile_set_position(gpsm_swfile_t *swfile, float position)
{
	if (!root || !swfile || !GPSM_ITEM_IS_SWFILE(swfile))
		return;
	swfile->position = position;
	glsig_emit(gpsm_item_emitter(swfile), GPSM_SIG_ITEM_CHANGED, swfile);
}



/*
 * Connection with lowlevel swapfile API.
 */

void gpsm_notify_swapfile_change(long filename, long pos, long size)
{
	gpsm_swfile_t *swfile = NULL;
#ifdef DEBUG
	swfd_t fd;
	struct sw_stat st;
	long file_size;

	if ((fd = sw_open(filename, O_RDONLY)) == -1) {
		DPRINTF("Invalid filename\n");
		return;
	}
	sw_fstat(fd, &st);
	file_size = st.size/SAMPLE_SIZE;
	sw_close(fd);
#endif

	/* Loop over all references sending out the changed signal. */
	while ((swfile = hash_find_next_swfile(filename, swfile))) {
		/* Handle extension transparently. */
		if (pos + size > gpsm_item_hsize(swfile))
			gpsm_notify_swapfile_insert(filename, gpsm_item_hsize(swfile), pos + size - 1 - gpsm_item_hsize(swfile));

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

	if ((fd = sw_open(filename, O_RDONLY)) == -1) {
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

	if ((fd = sw_open(filename, O_RDONLY)) == -1) {
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
	if ((fd = sw_open(filename, O_RDONLY)) == -1)
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

	if (!root || !GPSM_ITEM_IS_GRP(root) || !label)
		return NULL;

	if (!start) {
		item = glame_list_gethead(&root->items, gpsm_item_t, list);
	} else {
		item = start;
		goto next_entry;
	}

	while (item) {
		if (GPSM_ITEM_IS_GRP(item)
		    && strcmp(item->label, label) == 0)
			return (gpsm_grp_t *)item;
next_entry:
		if (GPSM_ITEM_IS_GRP(item)) {
			next = glame_list_gethead(&((gpsm_grp_t *)item)->items,
					    gpsm_item_t, list);
			if (!next)
				next = glame_list_getnext(&item->parent->items, item,
					    gpsm_item_t, list);
		} else
			next = glame_list_getnext(&item->parent->items, item,
					    gpsm_item_t, list);
		while (!next) {
			item = (gpsm_item_t *)item->parent;
			if (item == (gpsm_item_t *)root)
				return NULL;
			next = glame_list_getnext(&item->parent->items, item,
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

	if (!root || !GPSM_ITEM_IS_GRP(root) || !label)
		return NULL;

	if (!root || !GPSM_ITEM_IS_GRP(root))
		return NULL;

	if (!start) {
		item = glame_list_gethead(&root->items, gpsm_item_t, list);
	} else {
		item = start;
		goto next_entry;
	}

	while (item) {
		if (GPSM_ITEM_IS_SWFILE(item)
		    && strcmp(item->label, label) == 0)
			return (gpsm_swfile_t *)item;
next_entry:
		if (GPSM_ITEM_IS_GRP(item)) {
			next = glame_list_gethead(&((gpsm_grp_t *)item)->items,
					    gpsm_item_t, list);
			if (!next)
				next = glame_list_getnext(&item->parent->items, item,
					    gpsm_item_t, list);
		} else
			next = glame_list_getnext(&item->parent->items, item,
					    gpsm_item_t, list);
		while (!next) {
			item = (gpsm_item_t *)item->parent;
			if (item == (gpsm_item_t *)root)
				return NULL;
			next = glame_list_getnext(&item->parent->items, item,
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

	if (!root || !GPSM_ITEM_IS_GRP(root))
		return NULL;

	if (!start) {
		item = glame_list_gethead(&root->items, gpsm_item_t, list);
	} else {
		item = start;
		goto next_entry;
	}

	while (item) {
		if (GPSM_ITEM_IS_SWFILE(item)
		    && gpsm_swfile_filename(item) == filename)
			return (gpsm_swfile_t *)item;
next_entry:
		if (GPSM_ITEM_IS_GRP(item)) {
			next = glame_list_gethead(&((gpsm_grp_t *)item)->items,
					    gpsm_item_t, list);
			if (!next)
				next = glame_list_getnext(&item->parent->items, item,
					    gpsm_item_t, list);
		} else
			next = glame_list_getnext(&item->parent->items, item,
					    gpsm_item_t, list);
		while (!next) {
			item = (gpsm_item_t *)item->parent;
			if (item == (gpsm_item_t *)root)
				return NULL;
			next = glame_list_getnext(&item->parent->items, item,
					    gpsm_item_t, list);
		}
		item = next;
	}

	return NULL;
}

gpsm_swfile_t *gpsm_find_swfile_vposition(gpsm_grp_t *root, gpsm_item_t *start,
					  long vposition)
{
	gpsm_item_t *item, *next;

	if (!start) {
		item = glame_list_gethead(&root->items, gpsm_item_t, list);
	} else {
		item = start;
		goto next_entry;
	}

	while (item) {
		if (GPSM_ITEM_IS_SWFILE(item)
		    && gpsm_item_vposition(item) == vposition)
			return (gpsm_swfile_t *)item;
next_entry:
		if (GPSM_ITEM_IS_GRP(item)) {
			next = glame_list_gethead(&((gpsm_grp_t *)item)->items,
					    gpsm_item_t, list);
			if (!next)
				next = glame_list_getnext(&item->parent->items, item,
					    gpsm_item_t, list);
			else
				vposition -= gpsm_item_vposition(item);
		} else
			next = glame_list_getnext(&item->parent->items, item,
					    gpsm_item_t, list);
		while (!next) {
			item = (gpsm_item_t *)item->parent;
			vposition += gpsm_item_vposition(item);
			if (item == (gpsm_item_t *)root)
				return NULL;
			next = glame_list_getnext(&item->parent->items, item,
					    gpsm_item_t, list);
		}
		item = next;
	}

	return NULL;
}



/*
 * Complex operations.
 */

gpsm_grp_t *gpsm_flatten(gpsm_item_t *item)
{
	gpsm_grp_t *grp;
	gpsm_item_t *it, *next;
	char label[256];
	long hpos, vpos;

	if (!item)
		return NULL;

	snprintf(label, 255, "Flattened COW copy of %s",
		 gpsm_item_label(item));
	if (!(grp = gpsm_newgrp(label)))
		return NULL;

	/* Handle the special case of swfile item first. */
	if (GPSM_ITEM_IS_SWFILE(item)) {
		gpsm_swfile_t *cowi;

		if (!(cowi = gpsm_swfile_cow((gpsm_swfile_t *)item)))
			goto fail;
		gpsm_item_place(grp, (gpsm_item_t *)cowi, 0, 0);
		return grp;
	}

	/* The rest is for the usual case of item being a group.
	 * Traverse the subtree and insert the found swfiles into
	 * newly created ones in the grp. */
	hpos = vpos = 0;
	it = glame_list_gethead(&((gpsm_grp_t *)item)->items, gpsm_item_t, list);
	while (it) {
		/* If its a swfile, process it - remember, its position
		 * needs to be adjusted by hpos/vpos. */
		if (GPSM_ITEM_IS_SWFILE(it)) {
			gpsm_swfile_t *fswfile;
			swfd_t ffd, fd;
			struct sw_stat st, st2;
			long ithpos, itvpos;
			int res;

			ithpos = gpsm_item_hposition(it) + hpos;
			itvpos = gpsm_item_vposition(it) + vpos;
			fswfile = gpsm_find_swfile_vposition(grp, NULL, itvpos);
			if (!fswfile) {
				snprintf(label, 255, "Track %li", itvpos);
				if (!(fswfile = gpsm_newswfile(label)))
					goto fail;
				fswfile->position = gpsm_swfile_position(it);
				fswfile->samplerate = gpsm_swfile_samplerate(it);
				gpsm_item_place(grp, (gpsm_item_t *)fswfile,
						0, itvpos);
			}
			ffd = sw_open(gpsm_swfile_filename(fswfile), O_RDWR);
			fd = sw_open(gpsm_swfile_filename(it), O_RDONLY);
			sw_fstat(fd, &st);
			sw_fstat(ffd, &st2);
			if (ithpos*SAMPLE_SIZE + st.size > st2.size)
				sw_ftruncate(ffd, ithpos*SAMPLE_SIZE + st.size);
			sw_lseek(ffd, ithpos*SAMPLE_SIZE, SEEK_SET);
			res = sw_sendfile(ffd, fd, st.size, 0);
			sw_close(fd);
			sw_close(ffd);
			if (res == -1)
				goto fail;
		}

		/* Find the next item to be processed - looks
		 * complicated, but thats iterative tree traversal. */
		if (GPSM_ITEM_IS_GRP(it)) {
			next = glame_list_gethead(&((gpsm_grp_t *)it)->items,
					    gpsm_item_t, list);
			if (!next)
				next = glame_list_getnext(&it->parent->items, it,
						    gpsm_item_t, list);
			else {
				hpos += gpsm_item_hposition(it);
				vpos += gpsm_item_vposition(it);
			}
		} else
			next = glame_list_getnext(&it->parent->items, it,
					    gpsm_item_t, list);
		while (!next) {
			it = (gpsm_item_t *)it->parent;
			if (it == item)
				break;
			hpos -= gpsm_item_hposition(it);
			vpos -= gpsm_item_vposition(it);
			next = glame_list_getnext(&it->parent->items, it,
					    gpsm_item_t, list);
		}
		it = next;
	}

	/* Fix the length of all swfiles - set it to the maximum
	 * (which is the size of the original group). */
	gpsm_grp_foreach_item(grp, it) {
		swfd_t fd;
		if (!GPSM_ITEM_IS_SWFILE(it))
			continue;
		fd = sw_open(gpsm_swfile_filename(it), O_RDWR);
		sw_ftruncate(fd, gpsm_item_hsize(item)*SAMPLE_SIZE);
		sw_close(fd);
		gpsm_invalidate_swapfile(gpsm_swfile_filename(it));
	}

	return grp;

 fail:
	DPRINTF("failed!\n");
	gpsm_item_destroy((gpsm_item_t *)grp);
	return NULL;
}

gpsm_grp_t *gpsm_collect_swfiles(gpsm_item_t *item)
{
	gpsm_swfile_t **files;
	gpsm_item_t *it;
	gpsm_grp_t *grp;
	int i, cnt;
	char s[256];
	long hpos, vpos;

	/* Get the files. */
	cnt = _gpsm_get_swfiles(item, &files);
	if (cnt == 0)
		return NULL;

	/* Create a new group and insert them. */
	snprintf(s, 255, "Collected files of %s", item->label);
	grp = gpsm_newgrp(s);
	for (i=0; i<cnt; i++) {
		/* Start with 0, 0 as the swfiles position
		 * go up the tree until we reach item. */
		hpos = 0;
		vpos = 0;
		it = (gpsm_item_t *)files[i];
		while (it != item) {
			hpos += gpsm_item_hposition(it);
			vpos += gpsm_item_vposition(it);
			it = (gpsm_item_t *)gpsm_item_parent(it);
		}

		/* Create a link to the swfile and insert it into
		 * grp at the flattened position. */
		it = (gpsm_item_t *)gpsm_swfile_link(files[i]);
		gpsm_item_place(grp, it, hpos, vpos);
	}

	return grp;
}




/* Creates an array of all swfiles in the subtree item and stores it in
 * *files, returns the number of swfiles found. Free *files yourself. */
static int _gpsm_get_swfiles(gpsm_item_t *root, gpsm_swfile_t ***files)
{
	gpsm_swfile_t *_files[256];
	gpsm_item_t *item, *next;
	int cnt, c;

	/* Easy case first. */
	if (GPSM_ITEM_IS_SWFILE(root)) {
		*files = (gpsm_swfile_t **)malloc(sizeof(void *));
		(*files)[0] = (gpsm_swfile_t *)root;
		return 1;
	}

	cnt = 0;
	item = glame_list_gethead(&((gpsm_grp_t *)root)->items, gpsm_item_t, list);
	while (item) {
		if (GPSM_ITEM_IS_SWFILE(item)) {
			_files[cnt++] = (gpsm_swfile_t *)item;
			if (cnt > 255)
				PANIC("Max nr of swfiles reached");
		}
		if (GPSM_ITEM_IS_GRP(item)) {
			next = glame_list_gethead(&((gpsm_grp_t *)item)->items,
					    gpsm_item_t, list);
			if (!next)
				next = glame_list_getnext(&item->parent->items, item,
						    gpsm_item_t, list);
		} else
			next = glame_list_getnext(&item->parent->items, item,
					    gpsm_item_t, list);
		while (!next) {
			item = (gpsm_item_t *)item->parent;
			if (item == (gpsm_item_t *)root)
				goto done;
			next = glame_list_getnext(&item->parent->items, item,
					    gpsm_item_t, list);
		}
		item = next;
	}
 done:

	if (cnt == 0) {
		*files = NULL;
		return 0;
	}
	*files = (gpsm_swfile_t **)malloc(cnt*sizeof(void *));
	c = cnt;
	while (c--)
		(*files)[c] = _files[c];
	return cnt;
}



/*
 * Undo/Redo aka operations support.
 */

int gpsm_op_prepare(gpsm_item_t *item)
{
	struct op *op;

	if (!item)
		return -1;

	/* We need to possibly kill off a redo-record. */
	if ((op = _op_get(item)))
		_op_kill_redo(op);

	/* Start new op. */
	if (!(op = _op_prepare(item)))
		return -1;
	op->is_undo = 0;

	if (_op_cow(op) == -1) {
		free(op);
		return -1;
	}

	_op_add(op);

	return 0;
}

int gpsm_op_can_undo(gpsm_item_t *item)
{
	struct op *op;

	if (!item)
		return 0;

	/* Get the top operation, if its not redo we can undo. */
	op = _op_get(item);
	if (!op)
		return 0;
	if (!op->is_undo)
		return 1;

	/* If there is underlying undo, we can undo, too. */
	return _op_get_prev(op) ? 1 : 0;
}

int gpsm_op_can_redo(gpsm_item_t *item)
{
	struct op *op;

	if (!item)
		return 0;

	op = _op_get(item);
	if (!op || !op->is_undo)
		return 0;

	return 1;
}

int gpsm_op_undo(gpsm_item_t *item)
{
	struct op *op, *redo;

	if (!item || !(op = _op_get(item)))
		return -1;

	/* Kill possible redos. */
	op = _op_kill_redo(op);
	if (op->is_undo)
		return -1;

	/* Prepare for redo. */
	if (!(redo = _op_prepare(item)))
		return -1;
	redo->is_undo = 1;
	if (_op_cow(redo) == -1) {
		free(redo);
		return -1;
	}
	_op_add(redo);

	/* Undo. */
	if (_op_undo(op) == -1) {
		_op_delete(redo);
		return -1;
	}
	_op_delete(op);

	return 0;
}

int gpsm_op_undo_and_forget(gpsm_item_t *item)
{
	struct op *op;

	if (!item || !(op = _op_get(item)))
		return -1;

	/* Kill possible redos. */
	op = _op_kill_redo(op);
	if (op->is_undo)
		return -1;

	/* Undo. */
	if (_op_undo(op) == -1)
		return -1;
	_op_delete(op);

	return 0;
}

int gpsm_op_redo(gpsm_item_t *item)
{
	struct op *op, *undo;

	if (!item || !(op = _op_get(item)) || !(op->is_undo))
		return -1;

	/* Prepare for undo. */
	if (!(undo = _op_prepare(item)))
		return -1;
	undo->is_undo = 0;
	if (_op_cow(undo) == -1) {
		free(undo);
		return -1;
	}
	_op_add(undo);

	/* Redo. */
	if (_op_undo(op) == -1) {
		_op_delete(undo);
		return -1;
	}
	_op_delete(op);

	return 0;
}

int gpsm_op_redo_and_forget(gpsm_item_t *item)
{
	struct op *op;

	if (!item || !(op = _op_get(item)) || !(op->is_undo))
		return -1;

	if (_op_undo(op) == -1)
		return -1;
	_op_delete(op);

	return 0;
}

int gpsm_op_forget(gpsm_item_t *item)
{
	struct op *op;

	if (!item || !(op = _op_get(item)))
		return -1;

	_op_delete(op);

	return 0;
}




/*
 * Undo/Redo helpers.
 */

/* Allocates a new operation with room for nrpairs pairs and assigns
 * an unique time. */
static struct op *_op_new(int nrpairs)
{
	struct op *op;

	op = (struct op *)malloc(sizeof(struct op)
				 + nrpairs*sizeof(struct pair));
	GLAME_INIT_LIST_HEAD(&op->list);
	gettimeofday(&op->time, NULL);
	op->is_undo = 0;
	op->nrpairs = nrpairs;

	return op;
}

/* Deletes the operation op. Removes it from the oplist, if necessary,
 * deletes all saved state files and fixes all referenced swfiles to
 * point to their latest operation available. */
static void _op_delete(struct op *op)
{
	int i;

	if (!glame_list_empty(&op->list)) {
		glame_list_del(&op->list);
		op_listsize--;
	}

	/* Fix all refed swfiles. */
	_op_fix_swfiles(op);

	/* Loop through all pairs, killing the saved files. */
	for (i=0; i<op->nrpairs; i++) {
		if (op->pair[i].saved == -1)
			continue;
		sw_unlink(op->pair[i].saved);
	}

	free(op);
}

/* Kills overlapping redos (if an underlying undo is available),
 * also partial ones. */
static struct op *_op_kill_redo(struct op *op)
{
	struct op *prev, *op2;
	int i;

	if (op->is_undo) {
		/* If there is no prev op, do nothing, else kill off
		 * the redo and return the prev. */
		if (!(prev = _op_get_prev(op)))
			return op;
		_op_delete(op);
		return prev;
	}

	/* Now we need to kill "partially overlapping" redos. */
	for (i=0; i<op->nrpairs; i++) {
		op2 = _op_find_filename(op->pair[i].file);
		if (op2 != op)
			_op_delete(op2);
	}

	return op;
}

/* Finds the operation containing the saved swapfile file with the
 * provided filename filename. Returns NULL, if there is no such
 * operation. */
static struct op *_op_find_saved(long filename)
{
	struct op *op;
	int i;

	op = (struct op *)&oplist;
	while ((op = glame_list_getnext(&oplist, op, struct op, list))) {
		for (i=0; i<op->nrpairs; i++)
			if (op->pair[i].saved == filename)
				return op;
	}

	return NULL;
}

/* Finds the latest operation referencing the provided filename starting
 * at operation op. Returns NULL, if there is no such operation. */
static struct op *_op_find_filename_before(long filename, struct op *op)
{
	int i;

	while ((op = glame_list_getnext(&oplist, op, struct op, list))) {
		for (i=0; i<op->nrpairs; i++)
			if (op->pair[i].file == filename)
				return op;
	}

	return NULL;
}

/* Finds the latest operation referencing the provided filename. Returns
 * NULL, if there is no such operation. */
static struct op *_op_find_filename(long filename)
{
	/* HACK - but does work with above implementation. */
	return _op_find_filename_before(filename, (struct op *)&oplist);
}

/* Check if an operation has a previous "correct" operation (can only
 * be undo). */
static struct op *_op_get_prev(struct op *op)
{
	struct op *prev;
	int i, j;

	/* The first prev is good enough. */
	prev = _op_find_filename_before(op->pair[0].file, op);
	if (!prev)
		return NULL;

	/* Counts have to match. */
	if (prev->nrpairs != op->nrpairs)
		return NULL;

	/* Now we have to check there are no ops "inbetween". */
	for (i=1; i<op->nrpairs; i++)
		if (_op_find_filename_before(op->pair[i].file, op) != prev)
			return NULL;

	/* And we have to check for exact match of both file sets. */
	for (i=0; i<op->nrpairs; i++) {
		for (j=0; j<prev->nrpairs; j++)
			if (op->pair[i].file == prev->pair[j].file)
				break;
		if (j == prev->nrpairs)
			return NULL;
	}
	for (i=0; i<prev->nrpairs; i++) {
		for (j=0; j<op->nrpairs; j++)
			if (prev->pair[i].file == op->pair[j].file)
				break;
		if (j == op->nrpairs)
			return NULL;
	}

	return prev;
}

/* Return the pending redo operation of the provided files, if
 * available, or NULL. */
struct op *_op_get_redo(gpsm_swfile_t **files, int cnt)
{
	struct op *op;
	int i, j;

	/* As redo is always "on top", if it is redo, its also
	 * the most recent op of file 0. */
	op = _op_find_filename(gpsm_swfile_filename(files[0]));
	if (!op || !op->is_undo)
		return NULL;

	/* We need to check, if the file sets are equal. It should
	 * be enough to check for equality of the array sizes here,
	 * as else something else is broken, too. */
	if (op->nrpairs != cnt)
		return NULL;

	/* As redo is always "on top", equal times are now required
	 * for all swfiles. */
	for (i=1; i<cnt; i++)
		if (!timercmp(&files[i]->last_op_time,
			      &files[0]->last_op_time, ==))
			return NULL;

	/* We need to check, if the file sets are equal. */
	for (i=0; i<cnt; i++) {
		for (j=0; j<op->nrpairs; j++)
			if (op->pair[j].file == files[i]->filename)
				break;
		if (j == op->nrpairs)
			return NULL;
	}
	for (j=0; j<op->nrpairs; j++) {
		for (i=0; i<cnt; i++)
			if (op->pair[j].file == files[i]->filename)
				break;
		if (i == cnt)
			return NULL;
	}

	return op;

}

/* Return the pending undo operation of the provided files, if
 * available, or NULL. */
struct op *_op_get_undo(gpsm_swfile_t **files, int cnt)
{
	struct op *op, *op2;
	int i, j;

	/* As undo is not required to be "on top", but one level
	 * of redo (different op on each swfile possible!) is
	 * allowed, we need to find the undo first, i.e. start
	 * with file 0, possibly skip redo and check for each
	 * other file, if there is the same und op pending.
	 */

	/* Start with file 0 and find the only possible candidate
	 * for the pending undo op. */
	op = _op_find_filename(gpsm_swfile_filename(files[0]));
	if (!op)
		return NULL;
	if (op->is_undo)
		op = _op_find_filename_before(gpsm_swfile_filename(files[0]), op);
	if (!op)
		return NULL;

	/* We need to check, if the file sets are equal. It should
	 * be enough to check for equality of the array sizes here,
	 * as else something else is broken, too. */
	if (op->nrpairs != cnt)
		return NULL;

	/* Check the other files - matching timestamp is good enough,
	 * matching op, too, of course. */
	for (i=1; i<cnt; i++) {
		if (timercmp(&files[i]->last_op_time,
			     &files[0]->last_op_time, ==))
			continue;
		/* Ok, so skip the first op (which needs to be
		 * a redo) and match the two ops. */
		op2 = _op_find_filename(files[i]->filename);
		if (!op2 || !op2->is_undo)
			return NULL;
		op2 = _op_find_filename_before(files[i]->filename, op2);
		if (op2 != op)
			return NULL;
	}

	/* We need to check, if the file sets are equal. */
	for (i=0; i<cnt; i++) {
		for (j=0; j<op->nrpairs; j++)
			if (op->pair[j].file == files[i]->filename)
				break;
		if (j == op->nrpairs)
			return NULL;
	}
	for (j=0; j<op->nrpairs; j++) {
		for (i=0; i<cnt; i++)
			if (op->pair[j].file == files[i]->filename)
				break;
		if (i == cnt)
			return NULL;
	}

	return op;
}

/* Get the latest operation available for the subtree item. Returns
 * an operation, if exists and is complete. Else returns NULL. */
struct op *_op_get(gpsm_item_t *item)
{
	gpsm_swfile_t **files;
	struct op *op;
	int cnt;

	/* Get the files of the subtree, reject empty. */
	cnt = _gpsm_get_swfiles(item, &files);
	if (cnt == 0)
		return NULL;

	/* Redo pending? */
	if ((op = _op_get_redo(files, cnt))) {
		free(files);
		return op;
	}

	/* Undo pending? */
	if ((op = _op_get_undo(files, cnt))) {
		free(files);
		return op;
	}

	free(files);
	return NULL;
}

/* Alloc, init, but do _not_ add to oplist, fix all swfiles nor cow. */
static struct op *_op_prepare(gpsm_item_t *item)
{
	struct op *op;
	gpsm_swfile_t **files;
	int cnt, i;

	cnt = _gpsm_get_swfiles(item, &files);
	if (cnt == 0)
		return NULL;
	op = _op_new(cnt);
	for (i=0; i<cnt; i++) {
		op->pair[i].file = gpsm_swfile_filename(files[i]);
		op->pair[i].saved = -1;
	}
	free(files);

	return op;
}

/* Do the state saving operation on the operation op. For each referenced
 * swapfile cow the file to a new (pairs saved) file. Returns 0 on success,
 * -1 on error (where everything is undone). */
static int _op_cow(struct op *op)
{
	swfd_t source, dest;
	struct sw_stat st;
	int i;

	for (i=0; i<op->nrpairs; i++) {
		source = sw_open(op->pair[i].file, O_RDONLY);
		if (source == -1)
			goto err;
		while ((dest = sw_open(op->pair[i].saved = rand(),
				       O_WRONLY|O_CREAT|O_EXCL)) == -1)
			;
		sw_fstat(source, &st);
		if (sw_sendfile(dest, source, st.size, SWSENDFILE_INSERT) == -1) {
			sw_close(dest);
			sw_close(source);
			goto err;
		}
		sw_close(dest);
		sw_close(source);
	}

	return 0;

 err:
	DPRINTF("Error.");
	for (i=0; i<op->nrpairs; i++) {
		if (op->pair[i].saved == -1)
			continue;
		sw_unlink(op->pair[i].saved);
		op->pair[i].saved = -1;
	}
	return -1;
}

/* Adds the op to the oplist, adjusts the lists size and possibly
 * kills off ops from the tail. Fixes all swfiles referencing one
 * of the ops files. */
static void _op_add(struct op *op)
{
	glame_list_add(&op->list, &oplist);
	_op_fix_swfiles(op);
	op_listsize++;
	while (op_listsize > op_max_listsize) {
		DPRINTF("deleting too big list\n");
		op = glame_list_gettail(&oplist, struct op, list);
		_op_delete(op);
	}
}

/* Fix all swfiles last_op_time referenced by the operation op by
 * searching for the most recent available op in the oplist. */
static void _op_fix_swfiles(struct op *op)
{
	int i;

	for (i=0; i<op->nrpairs; i++) {
		gpsm_swfile_t *swfile = NULL;
		struct op *recent = _op_find_filename(op->pair[i].file);
		while ((swfile = hash_find_next_swfile(op->pair[i].file,
						       swfile))) {
			if (!recent)
				swfile->last_op_time = (struct timeval){0,0};
			else
				swfile->last_op_time = recent->time;
		}
	}
}

/* Undo the operation op: for each pair restore the saved state and
 * invalidate the swapfile. Does not unlink the saved states (done
 * by _op_delete). */
static int _op_undo(struct op *op)
{
	swfd_t file = -1, saved = -1;
	struct sw_stat st;
	int i;

	for (i=0; i<op->nrpairs; i++) {
		file = sw_open(op->pair[i].file, O_RDWR);
		saved = sw_open(op->pair[i].saved, O_RDONLY);
		if (file == -1 || saved == -1)
			goto err;
		if (sw_fstat(saved, &st) == -1
		    || sw_ftruncate(file, 0) == -1)
			goto err;
		if (sw_sendfile(file, saved, st.size, SWSENDFILE_INSERT) == -1) {
			perror("sendfile failed");
			goto err;
		}
		sw_close(saved);
		sw_close(file);
		gpsm_invalidate_swapfile(op->pair[i].file);
	}

	return 0;

 err:
	sw_close(file);
	sw_close(saved);
	return -1;
}



void gpsm_position_transform(gpsm_item_t *source, gpsm_grp_t *dest,
			     long *hpos, long *vpos)
{
	gpsm_grp_t *grp;

	/* First source to "global" transformation. */
	*hpos = 0;
	*vpos = 0;
	grp = gpsm_item_parent(source);
	while (gpsm_item_parent(grp)) {
		*hpos += gpsm_item_hposition(grp);
		*vpos += gpsm_item_vposition(grp);
		grp = gpsm_item_parent(grp);
	}

	/* Second, backwards "to local" transformation. */
	grp = dest;
	while (gpsm_item_parent(grp)) {
		*hpos -= gpsm_item_hposition(grp);
		*vpos -= gpsm_item_vposition(grp);
		grp = gpsm_item_parent(grp);
	}
}
