/*
 * glame_gui_utils.c
 *
 * $Id: glame_gui_utils.c,v 1.29 2003/04/15 19:00:40 richi Exp $
 *
 * Copyright (C) 2001 Johannes Hirche
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

#include <sys/param.h>
#include <values.h>
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#ifdef HAVE_LIBGLADE
#include <glade/glade.h>
#endif
#include "glmid.h"
#include "gpsm.h"
#include "glame_curve.h"
#include "edit_filter/filtereditgui.h"
#include "glame_param.h"
#include "glame_gui_utils.h"



/*
 * Widget creation helpers. GNOME/gtk+ suck...
 */


/* Generic callback to update a 256 chars size destination buffer from
 * a GtkEditable (such as gnome_entry's gtk_edit widget). */
static void update_string_from_editable_cb(GtkEditable *w, char *buf)
{
	char *chars = gtk_editable_get_chars(w, 0, -1);
        strncpy(buf, chars, 255);
	g_free(chars);
}

static void update_long_from_adjustment_cb(GtkAdjustment *adj, long *value)
{
	*value = adj->value;
}

static void update_double_from_adjustment_cb(GtkAdjustment *adj, double *value)
{
	*value = adj->value;
}

void create_label_widget_pair(GtkWidget *vbox,
			      const char *clabel, GtkWidget *w)
{
	GtkWidget *hbox, *label;
	hbox = gtk_hbox_new(TRUE,5);
	gtk_container_add(GTK_CONTAINER(vbox), hbox);
	label = gtk_label_new(clabel);
	gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
	gtk_container_add(GTK_CONTAINER(hbox), label);
	gtk_container_add(GTK_CONTAINER(hbox), w);
	gtk_widget_show_all(hbox);
}

void create_label_edit_pair(GtkWidget *vbox,
			    const char *label, const char *history,
			    char *result)
{
	GtkWidget *whbox, *wlabel, *wentry;

	whbox = gtk_hbox_new(TRUE, 5);

	wlabel = gtk_label_new(label);
	gtk_misc_set_alignment(GTK_MISC(wlabel), 1.0, 0.5);

	wentry = gnome_entry_new(history);
	gtk_entry_set_text(GTK_ENTRY(gnome_entry_gtk_entry(GNOME_ENTRY(wentry))), result);
	gtk_signal_connect(GTK_OBJECT(gnome_entry_gtk_entry(GNOME_ENTRY(wentry))), "changed", update_string_from_editable_cb, result);

	gtk_container_add(GTK_CONTAINER(whbox), wlabel);
	gtk_container_add(GTK_CONTAINER(whbox), wentry);
	gtk_container_add(GTK_CONTAINER(vbox), whbox);
	gtk_widget_show(wlabel);
	gtk_widget_show(wentry);
	gtk_widget_show(whbox);
}

void create_label_long_pair(GtkWidget *vbox,
			    const char *label, long *value,
			    long vmin, long vmax)
{
	GtkWidget *hbox, *l, *sbutton;
	GtkObject *adj;

	*value = MAX(vmin, MIN(vmax, *value));

	hbox = gtk_hbox_new(TRUE, 5);
	l = gtk_label_new(label);
	gtk_misc_set_alignment(GTK_MISC(l), 1.0, 0.5);
	adj = gtk_adjustment_new(*value, vmin, vmax, 1.0, 10.0, 0.0);
	sbutton = gtk_spin_button_new(GTK_ADJUSTMENT(adj), 1.0, 0);
	gtk_signal_connect(GTK_OBJECT(adj), "value_changed",
			   (GtkSignalFunc)update_long_from_adjustment_cb, value);
	gtk_container_add(GTK_CONTAINER(hbox), l);
	gtk_container_add(GTK_CONTAINER(hbox), sbutton);
	gtk_widget_show_all(hbox);
	gtk_container_add(GTK_CONTAINER(vbox), hbox);
}

void create_label_double_pair(GtkWidget *vbox,
			      const char *label, double *value,
			      double vmin, double vmax)
{
	GtkWidget *hbox, *l, *sbutton;
	GtkObject *adj;

	*value = MAX(vmin, MIN(vmax, *value));

	hbox = gtk_hbox_new(TRUE, 5);
	l = gtk_label_new(label);
	gtk_misc_set_alignment(GTK_MISC(l), 1.0, 0.5);
	adj = gtk_adjustment_new(*value, vmin, vmax, 0.1, 1.0, 0.0);
	sbutton = gtk_spin_button_new(GTK_ADJUSTMENT(adj), 1.0, 3);
	gtk_signal_connect(GTK_OBJECT(adj), "value_changed",
			   (GtkSignalFunc)update_double_from_adjustment_cb, value);
	gtk_container_add(GTK_CONTAINER(hbox), l);
	gtk_container_add(GTK_CONTAINER(hbox), sbutton);
	gtk_widget_show_all(hbox);
	gtk_container_add(GTK_CONTAINER(vbox), hbox);
}


static void glame_dialog_file_request_browse_cb(GnomeFileEntry *fileentry,
						const char *pattern)
{
	/* Well, this f*** widget is NULL here... */
	gtk_file_selection_complete(GTK_FILE_SELECTION(fileentry->fsw),
				    pattern);
}
GtkWidget *glame_dialog_file_request(const char *windowtitle,
				     const char *history_id, const char *label,
				     const char *pattern,
				     char *returnbuffer)
{
	GtkWidget *dialog;
	GtkWidget *fileEntry;
	GtkWidget *dialogVbox;	
	
	dialog = gnome_dialog_new(windowtitle, GNOME_STOCK_BUTTON_CANCEL,
				  GNOME_STOCK_BUTTON_OK, NULL);
        dialogVbox = GTK_WIDGET(GTK_VBOX(GNOME_DIALOG(dialog)->vbox));

        fileEntry = gnome_file_entry_new(history_id, label);
	gnome_file_entry_set_modal(GNOME_FILE_ENTRY(fileEntry), TRUE);
        gtk_signal_connect(GTK_OBJECT(gnome_file_entry_gtk_entry(GNOME_FILE_ENTRY(fileEntry))),
                           "changed", update_string_from_editable_cb,
			   returnbuffer);
	if (pattern)
		gtk_signal_connect_after(GTK_OBJECT(fileEntry), "browse-clicked",
					 (GtkSignalFunc)glame_dialog_file_request_browse_cb,
					 (gpointer)pattern);
        create_label_widget_pair(dialogVbox, "Filename", fileEntry);
	return dialog;
}


/*
 * Network error dialog
 */

void glame_network_error_dialog(filter_t *net, const char *header)
{
	filter_t *node;
	char msg[4096];
	int cnt = 0;

	cnt += snprintf(msg+cnt, 4096-cnt, "%s\n", header);
	filter_foreach_node(net, node) {
		if (filter_errstr(node))
			snprintf(msg+cnt, 4096-cnt, " %s: %s\n",
				 filter_name(node), filter_errstr(node));
	}
	gnome_dialog_run_and_close(GNOME_DIALOG(gnome_error_dialog(msg)));
}



/*
 * Plugin menu builder
 */

typedef struct {
	struct glame_list_head list;
	int is_category;
	union {
		struct {
			char *name;
			struct glame_list_head list;
		} c;
		struct {
			plugin_t *plugin;
		} p;
	} u;
} ggbpm_entry;
static ggbpm_entry *
glame_gui_build_plugin_menu_get_cat(struct glame_list_head *cats,
				    const char *catname)
{
	ggbpm_entry *cat;
	ggbpm_entry *before;
	char catn[256], *p;

	strncpy(catn, catname, 256);
	if ((p = strchr(catn, '/')))
		*p = '\0';

	/* Search for old category. */
	cat = glame_list_gethead(cats, ggbpm_entry, list);
	while (cat) {
		if (strcmp(cat->u.c.name, catn) == 0)
			goto recurse;
		cat = glame_list_getnext(cats, cat, ggbpm_entry, list);
	}

	/* Ok, need to create new one. */
	cat = (ggbpm_entry *)malloc(sizeof(ggbpm_entry));
	GLAME_INIT_LIST_HEAD(&cat->list);
	cat->is_category = 1;
	cat->u.c.name = strdup(catn);
	GLAME_INIT_LIST_HEAD(&cat->u.c.list);

	before = glame_list_gethead(cats, ggbpm_entry, list);
	while (before
	       && strcasecmp(cat->u.c.name, before->u.c.name) > 0)
		before = glame_list_getnext(cats, before, ggbpm_entry, list);
	if (!before)
		glame_list_add_tail(&cat->list, cats);
	else
		glame_list_add_tail(&cat->list, &before->list);

 recurse:
	if (!p)
		return cat;
	return glame_gui_build_plugin_menu_get_cat(&cat->u.c.list, p+1);
}
static void
glame_gui_build_plugin_menu_add_item(struct glame_list_head *cats, plugin_t *plugin)
{
	ggbpm_entry *entry;
	ggbpm_entry *cat;
	ggbpm_entry *before;
	const char *catname;

	/* Create entry for plugin with catname */
	entry = (ggbpm_entry *)malloc(sizeof(ggbpm_entry));
	GLAME_INIT_LIST_HEAD(&entry->list);
	entry->is_category = 0;
	entry->u.p.plugin = plugin;
	if (!(catname = plugin_query(plugin, PLUGIN_CATEGORY)))
		catname = "Default";

	/* Get cat. subtree */
	cat = glame_gui_build_plugin_menu_get_cat(cats, catname);

	/* Add the entry. */
	before = glame_list_gethead(&cat->u.c.list, ggbpm_entry, list);
	while (before
	       && strcasecmp(plugin_name(entry->u.p.plugin),
		             before->is_category
			       ? before->u.c.name
			       : plugin_name(before->u.p.plugin)) > 0)
		before = glame_list_getnext(&cat->u.c.list, before, ggbpm_entry, list);
	if (!before)
		glame_list_add_tail(&entry->list, &cat->u.c.list);
	else
		glame_list_add_tail(&entry->list, &before->list);
}
static void
glame_gui_build_plugin_menu_genmenu(struct glame_list_head *entries, GtkMenu *menu,
				    void (*gtksighand)(GtkWidget *, plugin_t *))
{
	ggbpm_entry *entry;

	while ((entry = glame_list_gethead(entries, ggbpm_entry, list))) {
		if (entry->is_category) {
			GtkWidget *citem = gtk_menu_item_new_with_label(
				entry->u.c.name);
			GtkWidget *cmenu = gtk_menu_new();
			gtk_menu_item_set_submenu(GTK_MENU_ITEM(citem), cmenu);
			gtk_menu_append(GTK_MENU(menu), citem);
			glame_gui_build_plugin_menu_genmenu(
				&entry->u.c.list, GTK_MENU(cmenu), gtksighand);
			gtk_widget_show(cmenu);
			gtk_widget_show(citem);
			free(entry->u.c.name);
		} else {
			const char *label;
			GtkWidget *mitem;
			label = plugin_query(entry->u.p.plugin, PLUGIN_LABEL);
			if (!label)
				label = plugin_name(entry->u.p.plugin);
			mitem = gtk_menu_item_new_with_label(label);
			gtk_widget_show(mitem);
			gtk_menu_append(GTK_MENU(menu), mitem);
			if (gtksighand)
				gtk_signal_connect(
					GTK_OBJECT(mitem), "activate",
					gtksighand, entry->u.p.plugin);
			gtk_widget_show(mitem);
		}
		glame_list_del(&entry->list);
		free(entry);
	}
}

/* Build a gtk menu from the registered plugins, selecting them
 * via callback. */
GtkMenu *glame_gui_build_plugin_menu(int (*select)(plugin_t *),
				     void (*gtksighand)(GtkWidget *, plugin_t *))
{
	GtkWidget *menu;
	plugin_t *plugin = NULL;
	GLAME_LIST_HEAD(categories);

	/* Build tree of selected categories/plugins. */
	while ((plugin = plugin_next(plugin))) {
		if (!select && !plugin_query(plugin, PLUGIN_FILTER))
			continue;
		if (select && !select(plugin))
			continue;
		glame_gui_build_plugin_menu_add_item(&categories, plugin);
	}

	/* Build the actual GtkMenu out of the tree (and free the tree
	 * while traversing it). */
	menu = gtk_menu_new();
	glame_gui_build_plugin_menu_genmenu(&categories, GTK_MENU(menu),
					    gtksighand);
	return GTK_MENU(menu);
}





/* creates a hbox with two labels in it, adds it to the box box in window win   */

void create_label_val_pair(GtkWidget *win,GtkWidget *box,const char *lab, const char* val)
{
	GtkWidget *hbox, *propval, *propname;
	
	propname = gtk_label_new(_(lab));
	propval = gtk_label_new(_(val));


	hbox = gtk_hbox_new (TRUE, 3);
	gtk_widget_ref (hbox);
	gtk_object_set_data_full (GTK_OBJECT (win), "hbox", hbox,
				  (GtkDestroyNotify) gtk_widget_unref);
	gtk_widget_show (hbox);
	gtk_box_pack_start(GTK_BOX (box), hbox,TRUE,TRUE,3);
	gtk_container_set_border_width(GTK_CONTAINER(hbox),2);

	gtk_widget_ref (propname);
	gtk_object_set_data_full (GTK_OBJECT (win), "probname", propname,
				  (GtkDestroyNotify) gtk_widget_unref);
	gtk_widget_show (propname);
	gtk_box_pack_start (GTK_BOX (hbox), propname, FALSE, FALSE, 0);
	

	gtk_widget_ref (propval);
	gtk_object_set_data_full (GTK_OBJECT (win), "probval", propval,
				  (GtkDestroyNotify) gtk_widget_unref);
	gtk_widget_show (propval);
	gtk_box_pack_start (GTK_BOX (hbox), propval, FALSE, FALSE, 0);
	
}

/* dito, but lab is a frame label here, and the value is inside the box  */
void create_frame_label_val_pair(GtkWidget *win,GtkWidget *box,const char *lab, const char* val)
{
	GtkWidget *propval,*frame;
	
	propval = gtk_label_new(_(val));

	frame = gtk_frame_new (_(lab));
	gtk_widget_ref (frame);
	gtk_object_set_data_full (GTK_OBJECT (win), "frame", frame,
				  (GtkDestroyNotify) gtk_widget_unref);

	gtk_widget_show (frame);
	gtk_box_pack_start (GTK_BOX (box), frame, TRUE, TRUE, 3);

	gtk_widget_ref (propval);
	gtk_object_set_data_full (GTK_OBJECT (win), "probval", propval,
				  (GtkDestroyNotify) gtk_widget_unref);
	gtk_widget_show (propval);
	gtk_container_add (GTK_CONTAINER (frame), propval);
	
}



void changeString(GtkEditable *wid, char *returnbuffer)
{
	char * chars;
	chars = gtk_editable_get_chars(wid,0,-1);
        strncpy(returnbuffer,chars,strlen(chars)+1);
}


static void fprop_kill_prop(glsig_handler_t *handler, long sig, va_list va)
{
	GtkWidget *widget = GTK_WIDGET(glsig_handler_private(handler));
	gtk_widget_destroy(widget);
}
static void fprop_kill_handler(GtkObject *object, glsig_handler_t *handler)
{
	glsig_delete_handler(handler);
}


void glame_help_cb(GtkWidget *bla, const char *helppath)
{
	char path[256];
	snprintf(path, 255, "info:glame#%s", helppath);
	gnome_help_goto(NULL, path);
}

GtkWidget *
glame_gui_filter_properties(filter_paramdb_t *pdb, const char *caption, const char *helppath)
{
	GtkWidget *vbox;
	GtkWidget* propBox;
	GtkWidget* tablabel;
	glsig_handler_t *handler;
	filter_param_t *param;

	propBox = gnome_property_box_new();

	tablabel = gtk_label_new(caption);
	vbox = glame_gui_from_paramdb(pdb);
	gnome_property_box_append_page(GNOME_PROPERTY_BOX(propBox),
				       vbox, tablabel);
	gtk_widget_show(vbox);

	gtk_object_destroy(GTK_OBJECT(GNOME_PROPERTY_BOX(propBox)->apply_button));
	if (helppath)
		gtk_signal_connect(GTK_OBJECT(GNOME_PROPERTY_BOX(propBox)->help_button), "clicked", glame_help_cb, (gpointer)helppath);
	else
		gtk_object_destroy(GTK_OBJECT(GNOME_PROPERTY_BOX(propBox)->help_button));
	gtk_object_destroy(GTK_OBJECT(GNOME_PROPERTY_BOX(propBox)->ok_button));
	gtk_window_set_modal(GTK_WINDOW(propBox), FALSE);

	/* Hook to the GLSIG_PARAM_DELETED signal of a random param in
	 * the paramdb and kill the property box (dont forget to kill
	 * the handler at property box delete time). */
	filterparamdb_foreach_param(pdb, param)
		break;
	if (param) { /* umm, else we are screwed? */
		handler = glsig_add_handler(filterparam_emitter(param),
					    GLSIG_PARAM_DELETED,
					    fprop_kill_prop, propBox);
		gtk_signal_connect(GTK_OBJECT(propBox), "destroy",
				   fprop_kill_handler, handler);
	}

	return propBox;
}

GtkWidget* glame_gui_multi_filter_properties(GList *items)
{
}	

GtkWidget *glame_gui_from_paramdb(filter_paramdb_t *pdb)
{
	GtkWidget *vbox, *gparam;
	filter_param_t *param;

	vbox = gtk_vbox_new(FALSE, 3);
	filterparamdb_foreach_param(pdb, param) {
		gparam = glame_param_new(param);
		if (!gparam)
			continue;
		gtk_container_add(GTK_CONTAINER(vbox), gparam);
		gtk_widget_show_all(gparam);
	}

	return vbox;
}





GdkPixbuf* glame_load_icon(const char *filename, int x, int y)
{
	GdkPixbuf* image = NULL;
	GdkPixbuf* imagetmp = NULL;
	const char * file;
	char * filepath;

	/* no filename given, ->default */
	if(!filename)
		file = GLAME_DEFAULT_ICON;
	else
		file = filename;

	/* check if stock gnome */
	if ((filepath = gnome_pixmap_file(file))) {
		imagetmp = gdk_pixbuf_new_from_file(filepath,NULL);
		g_free(filepath);
	}
	if(!imagetmp){
		/* maybe in Glamepixmappath? */
		filepath = g_concat_dir_and_file(GLAME_PIXMAP_PATH,file);
		if (!g_file_test(filepath, G_FILE_TEST_IS_REGULAR)) {
			g_free(filepath);
			/* maybe cvs? */
			filepath = g_concat_dir_and_file("../data/pixmaps", file);
		}
		if (!g_file_test(filepath, G_FILE_TEST_IS_REGULAR)) {
			g_free(filepath);
			/* default! */
			filepath = gnome_pixmap_file(GLAME_DEFAULT_ICON);
		}
		if(!filepath)
			filepath = g_concat_dir_and_file("../data/pixmaps", GLAME_DEFAULT_ICON);
		
		imagetmp = gdk_pixbuf_new_from_file(filepath,NULL);
		if(!imagetmp)
			DPRINTF("Failed to load image %s to pixbuf\n",filepath);
		g_free(filepath);
	}
	if(x && y){
		if(!imagetmp)
			DPRINTF("pixbuf == NULL\n");
		image = gdk_pixbuf_scale_simple(imagetmp,x,y,GDK_INTERP_BILINEAR);
		g_object_unref(imagetmp);
		return image;
	}else{
		return imagetmp;
	}
}

GtkWidget* glame_load_icon_widget(const char* filename,int x, int y)
{
	GdkPixbuf* img;

	img = glame_load_icon(filename,x,y);
	if (!img) {
		DPRINTF("Error opening %s\n", filename);
		return NULL;
	}
	return gtk_image_new_from_pixbuf(img);
}




/*
 * Async. network <-> GUI interaction and cleanup.
 */

struct network_notificator {
	glsig_emitter_t emitter;
	guint timeout_handler_id;
	glsig_handler_t *delete_handler;
	filter_t *net;
	int wbufsize;
	filter_launchcontext_t *context;
};

static void network_notificator_delete(glsig_handler_t *handler, long sig,
				       va_list va)
{
	struct network_notificator *n;
	n = (struct network_notificator *)glsig_handler_private(handler);

	/* Kill timeout handler. */
	gtk_timeout_remove(n->timeout_handler_id);

	/* Kill delete handler. */
	glsig_delete_handler(n->delete_handler);

	/* Run cleanup handlers. */
	glsig_emit(&n->emitter, GLSIG_NETWORK_DONE, n->net);

	/* Cleanup notificator. */
	glsig_delete_all(&n->emitter);

	filter_launchcontext_unref(&n->context);
	free(n);
}

static gint network_notificator_timeout(struct network_notificator *n)
{
	if (!filter_is_ready(n->context)) {
		/* Run tick handlers. */
		glsig_emit(&n->emitter, GLSIG_NETWORK_TICK, n->net);
		return TRUE;
	}

	/* Kill delete handler. */
	glsig_delete_handler(n->delete_handler);

	/* Run cleanup handlers. */
	glsig_emit(&n->emitter, GLSIG_NETWORK_DONE, n->net);

	/* Cleanup notificator. */
	glsig_delete_all(&n->emitter);

	filter_launchcontext_unref(&n->context);
	free(n);

	/* Remove timeout handler. */
	return FALSE;
}

glsig_emitter_t *glame_network_notificator_creat(filter_t *net)
{
	struct network_notificator *n;

	/* Init notification stuff. */
	n = ALLOC(struct network_notificator);
	INIT_GLSIG_EMITTER(&n->emitter);
	n->net = net;
	n->wbufsize = _GLAME_WBUFSIZE;
	n->context = NULL;

	return &n->emitter;
}

void glame_network_notificator_set_wbufsize(glsig_emitter_t *emitter,
					    int wbufsize)
{
	struct network_notificator *n = (struct network_notificator *)emitter;
	n->wbufsize = wbufsize;
}

int glame_network_notificator_run(glsig_emitter_t *emitter, int timeout)
{
	struct network_notificator *n = (struct network_notificator *)emitter;
	/* First launch & start the network. Cleanup in case of
	 * errors. */
	if (!(n->context = filter_launch(n->net, n->wbufsize))
	    || filter_start(n->context) == -1) {
		free(n);
		return -1;
	}

	/* Everything went ok, install gtk_timeout handler for polling,
	 * filter handler for deletion. */
	n->timeout_handler_id = gtk_timeout_add(
		timeout, (GtkFunction)network_notificator_timeout, n);
	n->delete_handler = glsig_add_handler(filter_emitter(n->net),
					      GLSIG_FILTER_DELETED,
					      network_notificator_delete, n);

	return 0;
}

void glame_network_notificator_delete_network(glsig_handler_t *handler,
					      long sig, va_list va)
{
	filter_t *net;

	GLSIGH_GETARGS1(va, net);

	filter_terminate(net->launch_context);
	filter_delete(net);
}

void glame_network_notificator_destroy_gpsm(glsig_handler_t *handler,
					    long sig, va_list va)
{
	gpsm_item_destroy((gpsm_item_t *)glsig_handler_private(handler));
}


gint glame_menu_get_active_index(GtkMenu *menu)
{
	GtkWidget *act;
	GList *list;
	int val;

	act = gtk_menu_get_active(menu);
	list = gtk_container_children(GTK_CONTAINER(menu));
	val = 0;
	while (list) {
		if ((GtkWidget *)(list->data) == act)
			break;
		list = g_list_next(list);
		val++;
	}

	return list ? val : -1;
}

void gnome_help_goto(void *ignore, gchar *file)
{
        char **ret;
        gnome_help_display_uri(file,ret);
}



