#include "glame_editor.h"
#include "swapfile.h"

static void load_sample_file(GtkWidget *bla, void*blu);
static void _load_sample(GtkWidget *bla, void*blu);
static void save_sample_file(GtkWidget *bla, void*blu);
static void save_as_sample_file(GtkWidget *bla, void*blu);
static void exit_glame_editor(GtkWidget *bla, void *blu) {
	gtk_main_quit();
};

GtkWidget *app = NULL;
GtkWidget *fileselect = NULL;
GtkWidget *ctree = NULL;
	
struct gledit_buffer {
	gchar	**label;
	long	swapfile_name;
	int	sample_rate;
	long	size;
	gboolean is_dir;
};

typedef struct gledit_buffer gledit_buffer_t;

GNode *global_bufs = NULL;

static gchar *buflabels[] = { "Filename", "Position", "Size", "Sample Rate" };
static gchar *poslabels[] = { "left", "right", "center" };

static GnomeUIInfo file_menu[]=
{
	GNOMEUIINFO_ITEM("_Load Sample...","Load Sample",load_sample_file,NULL),
	GNOMEUIINFO_MENU_SAVE_ITEM (save_sample_file, NULL),
	GNOMEUIINFO_MENU_SAVE_AS_ITEM (save_as_sample_file, NULL),
	GNOMEUIINFO_SEPARATOR,
	GNOMEUIINFO_MENU_EXIT_ITEM (exit_glame_editor, NULL),
	GNOMEUIINFO_END
};

/*
static GnomeUIInfo swapfile_menu[]=
{
};
*/

static GnomeUIInfo menubar[]=
{
	GNOMEUIINFO_MENU_FILE_TREE(file_menu),
	GNOMEUIINFO_END
};


void *make_label(char *filename, char *position, char *size, char *samplerate) {
	gchar **label;
	g_assert((label = g_malloc(sizeof(gchar *)*4))!=NULL);
	label[0] = filename;
	label[1] = position;
	label[2] = size;
	label[3] = samplerate;
	g_print("make_label: %s, %s, %s, %s\n", label[0], label[1], label[2], label[3]);
	return label;
};

GNode *new_buffer_gnode(gchar *name, long sname, float pos, int srate, long size, gboolean is_dir) {
	GNode		*dum;
	gledit_buffer_t	*gdum;
	gchar		*lpos, *lsize, *lsrate;
	g_assert((gdum = g_malloc(sizeof(gledit_buffer_t)))!=NULL);
	gdum->swapfile_name = sname;
	gdum->sample_rate = srate;
	gdum->size = size;
	gdum->is_dir = is_dir;
	if (!is_dir) {
		if (pos<0.0)
			lpos = g_strdup(poslabels[0]);
		else if (pos>0.0)
			lpos = g_strdup(poslabels[1]);
		else if (pos==0.0)
			lpos = g_strdup(poslabels[2]);
		else
			lpos = g_strdup_printf("%f", pos);
		lsize = g_strdup_printf("%d kByte", (gint)size/1024);
		lsrate = g_strdup_printf("%d Hz", srate);
		gdum->label = make_label(name, lpos, lsize, lsrate);
	} else gdum->label = make_label(name, NULL, NULL, NULL);
	
	dum = g_node_new(gdum);
	return dum;
};

gboolean update_buffer_gnode(GNode *gnode, gpointer bla) {
	swfd_t	fd;
	struct sw_stat fstat;
	gledit_buffer_t *gdum = gnode->data;

	fd = sw_open(gdum->swapfile_name, O_RDONLY, TXN_NONE);
	sw_fstat(fd, &fstat);
	sw_close(fd);
	free(gdum->label[2]);
	gdum->label[2] = g_strdup_printf("%d kByte", (gint)fstat.size/1024);
	return TRUE;
};

gboolean ctreefunc(GtkCTree     *ctree, 
		   guint         depth, 
		   GNode        *gnode, 
		   GtkCTreeNode *cnode, 
		   gpointer      data) {
	gint i;
	gledit_buffer_t *gdum = gnode->data;
	
	g_print("ctreefunc: depth = %d label=%s\n", depth, gdum->label[0]);
	gtk_ctree_set_node_info(ctree, cnode, gdum->label[0], 0, 
				NULL, NULL, NULL, NULL, !(gdum->is_dir), FALSE);
	
	if(!gdum->is_dir) {
		for(i=1; i<4; i++)
			gtk_ctree_node_set_text(ctree, cnode, i, gdum->label[i]);
	};
	
	return TRUE;
};

GtkWidget* gui_glame_editor_setup()
{
	GtkWidget *app;
	GnomeDock *dock;
	GtkWidget *sw;
	GNode *node, *node1;
	glong name;
	SWDIR	*dir;
	swfd_t	fd;
	struct sw_stat fstat;
	gint i;
	
	/* Application window with docks */
	
	app = gnome_app_new("Glame Editor", NULL);
	dock = GNOME_DOCK(GNOME_APP(app)->dock);
	
	gtk_window_set_default_size(GTK_WINDOW(app), 500, 300);
	gtk_widget_ref(GTK_WIDGET(dock));
	gtk_widget_show(GTK_WIDGET(dock));

	gnome_app_create_menus(GNOME_APP(app), menubar);
	gtk_widget_show(app);
	gtk_signal_connect(
			GTK_OBJECT(app), "delete-event",
			GTK_SIGNAL_FUNC(exit_glame_editor), NULL);

	/* Load Sample Fileselector */

	fileselect = gtk_file_selection_new("Load Sample");
	
	gtk_signal_connect(
			GTK_OBJECT(GTK_FILE_SELECTION(fileselect)->ok_button), "clicked", 
			GTK_SIGNAL_FUNC(_load_sample), NULL);
	gtk_signal_connect_object(
			GTK_OBJECT(GTK_FILE_SELECTION(fileselect)->cancel_button),"clicked",
			GTK_SIGNAL_FUNC(gtk_widget_hide),
			GTK_OBJECT(fileselect));

	/* Buffer List */

	sw = gtk_scrolled_window_new(NULL,  NULL);
	gtk_widget_show(sw);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(sw), 
				       GTK_POLICY_AUTOMATIC, 
				       GTK_POLICY_AUTOMATIC);

	ctree = gtk_ctree_new_with_titles(4, 0, buflabels);
	for(i=0;i<4;i++)
		gtk_clist_set_column_auto_resize(GTK_CLIST(ctree), i, TRUE);
	

	gtk_container_add(GTK_CONTAINER(sw), ctree);
	gnome_app_set_contents(GNOME_APP(app),sw);
	gtk_widget_show(ctree);
	
	/* buffer setup */

	global_bufs = new_buffer_gnode("Swapfiles", 0, 0.0, 0, 0, TRUE);
	node1 = new_buffer_gnode("Old Tracks", 0, 0.0, 0, 0, TRUE);
	g_node_append(global_bufs, node1);
	dir = sw_opendir();
	i = 0;
	if (dir!=NULL) {
		while((name = sw_readdir(dir))!=-1) {
			fd = sw_open(name, O_RDONLY, TXN_NONE);
			sw_fstat(fd, &fstat);
			sw_close(fd);
			node = new_buffer_gnode(g_strdup_printf("Track-%d", i), name, 0.0, 
						44100, fstat.size, FALSE);
			g_node_append(node1, node);
			i++;
		};
		sw_closedir(dir);
	};
	
	gtk_ctree_insert_gnode(GTK_CTREE(ctree), NULL, NULL, global_bufs, ctreefunc, NULL);
	g_print("Swapfile was already populated by %d files.\n", i);
	return app;
};

static void _load_sample(GtkWidget *bla, void*blu) {
	gchar *buffer;
	plugin_t *p_readfile, *p_swapfile_out;
	filter_t *net, *readfile, *swapfile_out[2]; /* lame stereo hack */
	filter_paramdb_t *db;
	filter_param_t *param;
	filter_port_t *source, *dest;
	filter_pipe_t *pipe;
	gint i, name;
	swfd_t fd;
	GNode *child, *parent;

	if (fileselect == NULL) {
		g_print("Guru File Select Meditation occured\n");
	} else {
		buffer = gtk_file_selection_get_filename(GTK_FILE_SELECTION(fileselect));
		g_print("Now we're loading %s into the swapfile\n", buffer);
		gtk_widget_hide(fileselect);
		

		if (!(p_readfile = plugin_get("read_file"))) {
			g_print("read_file not found");
			return;
		} else
			g_assert((readfile = filter_instantiate(p_readfile)));

		net = filter_creat(NULL);

		filter_add_node(net, readfile, "readfile");

		if (!(p_swapfile_out = plugin_get("swapfile_out"))) {
			g_print("swapfile_out not found");
			return;
		} 
		
		g_assert((db = filter_paramdb(readfile)));
		g_assert((param = filterparamdb_get_param(db, "filename")));
		filterparam_set(param, &buffer); 
		g_assert((source = filterportdb_get_port(filter_portdb(readfile), PORTNAME_OUT)));
		
		parent = new_buffer_gnode(g_strdup(buffer), -1, 0.0, 0, 0, TRUE);
		g_node_append(global_bufs, parent);

		for(i=0; i<2;i++) {
			name = 0;
			while((fd=sw_open(name, O_CREAT | O_EXCL, TXN_NONE))==-1) name++;
			sw_close(fd);
			g_print("Found free file at #%d\n", name);
			g_assert((swapfile_out[i] = filter_instantiate(p_swapfile_out)));
			g_assert((db = filter_paramdb(swapfile_out[i])));
			g_assert((param = filterparamdb_get_param(db, "filename")));
			filterparam_set(param, &name);
			filter_add_node(net, swapfile_out[i], "swapfile_out");
			g_assert((dest = filterportdb_get_port(filter_portdb(swapfile_out[i]), 
				 PORTNAME_IN)));
			if (!(filterport_connect(source, dest))) {
				g_print("Connection failed for channel %d\n",i+1);
				sw_unlink(name);
				filter_delete(swapfile_out[i]);
				goto launch;
			} else {
				pipe = filterport_get_pipe(dest);
				child = new_buffer_gnode(g_strdup(buffer), 
							 name, 
							 filterpipe_sample_hangle(pipe), 
							 filterpipe_sample_rate(pipe), 
							 0, FALSE);
				g_node_append(parent, child);
			}
		};
launch:
		filter_launch(net);
		filter_start(net);
		filter_wait(net);	/* ok we could do that more nicely, but not now.. */
		
		filter_delete(net);
		
		g_node_children_foreach(parent, G_TRAVERSE_ALL, update_buffer_gnode, NULL);

		gtk_ctree_insert_gnode(GTK_CTREE(ctree), 
				       gtk_ctree_node_nth(GTK_CTREE(ctree),0),
				       NULL, parent, ctreefunc, NULL);
	}
	
};

static void load_sample_file(GtkWidget *bla, void*blu) {
	if (fileselect == NULL)
		g_print("Strange, we don't have a fileselector :/");
	else
		gtk_widget_show(fileselect);
};

static void save_sample_file(GtkWidget *bla, void*blu) {};
static void save_as_sample_file(GtkWidget *bla, void*blu) {};
