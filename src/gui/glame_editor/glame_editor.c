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
GtkWidget *buffer_clist;

struct gledit_buffer {
	gchar	*filename;
	GSList	*swapfile_names;
	int	sample_rate;
};

typedef struct gledit_buffer gledit_buffer_t;

GList *global_bufs = NULL;

static gchar *buflabels[] = { "Filename", "Size", "Channels", "Sample Rate" };

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

void add_gledit_buffer_to_clist(gledit_buffer_t *buf) {
	gchar *data[4];
	GSList *elem;
	gint channels;
	glong size=0, name;
	swfd_t fd;
	struct sw_stat stat;
	elem = buf->swapfile_names;
	channels=0;
	while((elem!=NULL)) {
		channels++;
		name = (glong)elem->data;
		g_print("opening file %ld\n", name);
		fd = sw_open(name, O_RDONLY, TXN_NONE);
		sw_fstat(fd, &stat);
		sw_close(fd);
		size+=stat.size;
		g_print("size : %d\n", stat.size);
		elem=g_slist_next(elem);
	}
	
	size/=1024;
	g_print("channels: %d\n", channels);
	g_print("total filesize : %ld kbyte\n", size);
	
	data[0]=g_strdup(buf->filename);
	data[1]=g_strdup_printf("%ld kbyte", size);
	data[2]=g_strdup_printf("%d", channels);
	data[3]=g_strdup_printf("%d", buf->sample_rate);
	gtk_clist_append(GTK_CLIST(buffer_clist), data);
}

static GnomeUIInfo menubar[]=
{
	GNOMEUIINFO_MENU_FILE_TREE(file_menu),
	GNOMEUIINFO_END
};

GtkWidget* gui_glame_editor_setup()
{
	GtkWidget *app;
	GnomeDock *dock;
	GtkWidget *sw;
	glong name;
	swfd_t fd;
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

	buffer_clist = gtk_clist_new_with_titles(4, buflabels);
	gtk_clist_set_selection_mode(GTK_CLIST(buffer_clist), GTK_SELECTION_MULTIPLE);
	for(i=0;i<4;i++)
		gtk_clist_set_column_auto_resize(GTK_CLIST(buffer_clist), i, TRUE);
	gtk_widget_show(buffer_clist);
	gtk_container_add(GTK_CONTAINER(sw), buffer_clist);
	gnome_app_set_contents(GNOME_APP(app),sw);
	
	/* buffer setup */
	
	name = 0;
	while((fd=sw_open(name, O_RDONLY, TXN_NONE))!=-1) {
		gledit_buffer_t *gbuf;
		sw_close(fd);
		g_assert((gbuf = g_malloc(sizeof(gledit_buffer_t))));
		gbuf->filename = g_strdup_printf("Unknown-%d",name);
		gbuf->swapfile_names = g_slist_append(NULL, GINT_TO_POINTER(name));
		gbuf->sample_rate = 44100; /*sane default */
		add_gledit_buffer_to_clist(gbuf);
		global_bufs = g_list_append(global_bufs, gbuf);
		name++;
	}
	g_print("Swapfile was already populated by %ld files.\n", name);
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
	gledit_buffer_t *gbuf;
	gint i, name;
	swfd_t fd;
	GSList *head;
	
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
		
		g_assert((gbuf = g_malloc(sizeof(gledit_buffer_t))));
		gbuf->filename = g_strdup(buffer);
		gbuf->swapfile_names = head = NULL;
		
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
				head = g_slist_append(head, GINT_TO_POINTER(name));
				if (gbuf->swapfile_names==NULL) 
					gbuf->swapfile_names = head;
			}
		};
launch:
		filter_launch(net);
		filter_start(net);
		filter_wait(net);	/* ok we could do that more nicely, but not now.. */
		
		pipe = filterport_get_pipe(source);
		gbuf->sample_rate = filterpipe_sample_rate(pipe);
		global_bufs = g_list_append(global_bufs, gbuf);
		add_gledit_buffer_to_clist(gbuf);
			
		filter_delete(net);
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
