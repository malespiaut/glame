/*
 * glame_param.c
 *
 * $Id: glame_param.c,v 1.7 2001/07/31 16:16:52 mag Exp $
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

#include <values.h>
#include <gnome.h>
#ifdef HAVE_LIBGLADE
#include <glade/glade.h>
#endif
#include "glame_param.h"


static void glame_param_destroy(GtkObject *gparam)
{
	GtkHBox* parent_class;
	parent_class = gtk_type_class(GTK_TYPE_HBOX);
	GTK_OBJECT_CLASS(parent_class)->destroy(gparam);
	if (GLAME_PARAM(gparam)->fhandler) {
		glsig_delete_handler(GLAME_PARAM(gparam)->fhandler);
		GLAME_PARAM(gparam)->fhandler = NULL;
	}
}

static void glame_param_class_init(GlameParamClass *class)
{
	GtkObjectClass *object_class;
	object_class = GTK_OBJECT_CLASS(class);
	object_class->destroy = glame_param_destroy;
}

static void glame_param_init(GlameParam *gparam)
{
	gparam->param = NULL;
	gparam->fhandler = NULL;
	gparam->label = NULL;
	gparam->widget = NULL;
	gparam->u.widget = NULL;
	gparam->updating = 0;
}

GtkType glame_param_get_type(void)
{
	static GtkType glame_param_type = 0;
	
	if (!glame_param_type){
		GtkTypeInfo glame_param_info = {
			"GlameParam",
			sizeof(GlameParam),
			sizeof(GlameParamClass),
			(GtkClassInitFunc)glame_param_class_init,
			(GtkObjectInitFunc)glame_param_init,
			NULL,NULL,(GtkClassInitFunc)NULL,};
		glame_param_type = gtk_type_unique(
			GTK_TYPE_HBOX, &glame_param_info);
		gtk_type_set_chunk_alloc(glame_param_type, 8);
	}

	return glame_param_type;
}


static void handle_param(glsig_handler_t *handler, long sig, va_list va)
{
	GlameParam *gparam = GLAME_PARAM(glsig_handler_private(handler));

	if (sig == GLSIG_PARAM_DELETED) {
		DPRINTF("Got param delete event for %s\n",
			filterparam_label(gparam->param));
		gtk_object_destroy(GTK_OBJECT(gparam));
		return;
	}

	if (gparam->updating) {
		DPRINTF("Ignoring param change event for %s\n",
			filterparam_label(gparam->param));
		return;
	}
	DPRINTF("Got param change event for %s\n",
		filterparam_label(gparam->param));

	gparam->updating = 1;

	if (GTK_IS_ADJUSTMENT(gparam->u.widget)) {
		if (FILTER_PARAM_IS_FLOAT(gparam->param))
			gtk_adjustment_set_value(
				gparam->u.adj,
				filterparam_val_float(gparam->param));
		else if (FILTER_PARAM_IS_SAMPLE(gparam->param))
			gtk_adjustment_set_value(
				gparam->u.adj,
				filterparam_val_sample(gparam->param));
		else if (FILTER_PARAM_IS_INT(gparam->param))
			gtk_adjustment_set_value(
				gparam->u.adj,
				filterparam_val_int(gparam->param));
	} else if (GTK_IS_EDITABLE(gparam->u.widget)) {
		char *val;
		gint pos = 1;
		val = filterparam_to_string(gparam->param);
		DPRINTF("Updating editable to \"%s\"\n", val);
		gtk_editable_delete_text(gparam->u.edit, 0, -1);
		gtk_editable_set_position(gparam->u.edit, 0);
		/* This doesnt work!? -- GTK suxx again. */
		gtk_editable_insert_text(gparam->u.edit,
					 val, strlen(val), &pos);
		/* gtk_editable_changed(gparam->u.edit);
		   GTK_EDITABLE_CLASS(GTK_OBJECT(gparam->u.edit)->klass)->update_text(gparam->u.edit, 0, -1); */
		free(val);
	} else if (GLAME_IS_CURVE(gparam->u.widget)) {
		char *ctl_vec_string, *num_string;
		char * string_begin,*string_next;
		gfloat (*ctl_points)[2];
		int num_points,i;
		if ((num_string = filterparam_get_property(gparam->param, "curve-control-points"))
		    && (ctl_vec_string = filterparam_get_property(gparam->param, "curve-control-points-data"))) {
			num_points = atoi(num_string);
			fprintf(stderr,"num: %s %d\n%s\n",num_string,num_points,ctl_vec_string);
			ctl_points = malloc(num_points*sizeof(gfloat[2]));
			string_begin = ctl_vec_string;
			for(i=0; i<num_points; i++){
				ctl_points[i][0] = strtod(string_begin,&string_next);
				string_begin=string_next;
				ctl_points[i][1] = strtod(string_begin,&string_next);
				string_begin=string_next;
				/*sscanf(ctl_vec_string,"%f %f",&(ctl_points[i][0]),&(ctl_points[i][1]));*/
				/*fprintf(stderr,"set_param: %f %f\n",ctl_points[i][0],ctl_points[i][1]);*/
			}
			glame_curve_set_control_vector(
				gparam->u.curve, num_points, ctl_points);
		} else
			DPRINTF("No props for curve?\n");
	} else if (GTK_IS_OPTION_MENU(gparam->u.widget)) {
		gtk_option_menu_set_history(
			GTK_OPTION_MENU(gparam->u.widget),
			filterparam_val_int(gparam->param));
	} else
		DPRINTF("FIXME: unhandled widget type\n");

	gparam->updating = 0;
}

static gint editable_cb(GtkEditable *edit, GlameParam *gparam)
{
	gchar *text;
	int res;

	if (gparam->updating)
		return TRUE;

	text = gtk_editable_get_chars(edit, 0, -1);
	gparam->updating = 1;
	res = filterparam_set_string(gparam->param, text);
	gparam->updating = 0;
	g_free(text);

	return res == 0 ? TRUE : FALSE;
}

static gint curve_cb(GlameCurve* curve, GlameParam *gparam)
{
	int numpoints,i;
	gfloat (*ctlpoints)[2];
	char* ctlbuffer;
	filter_buffer_t *sbuf;

	sbuf = sbuf_alloc(1000, NULL);
	sbuf_make_private(sbuf);
	gtk_curve_get_vector(GTK_CURVE(gparam->u.curve),
			     sbuf_size(sbuf), sbuf_buf(sbuf));
	filterparam_set(gparam->param, &sbuf);
	sbuf_unref(sbuf);
	glame_curve_get_control_vector(gparam->u.curve,
				       &numpoints, &ctlpoints);
	ctlbuffer = calloc(numpoints*2*12, sizeof(gfloat));
	sprintf(ctlbuffer, "%d", numpoints);
	filterparam_set_property(gparam->param, "curve-control-points",
				 ctlbuffer);
	i=0;
	while(i<numpoints){
		sprintf(&ctlbuffer[i*28], "%13.8f %13.8f ",
			(ctlpoints)[i][0], (ctlpoints)[i][1]);
		i++;
	}
	fprintf(stderr,"ctl_strin in: %s\n", ctlbuffer);
	filterparam_set_property(gparam->param,
				 "curve-control-points-data",
				 ctlbuffer);
	g_free(ctlpoints);
	free(ctlbuffer);

	return TRUE;
}

static gint adjustment_cb(GtkAdjustment *adj, GlameParam *gparam)
{
	int res;

	if (gparam->updating)
		return TRUE;

	gparam->updating = 1;
	if (FILTER_PARAM_IS_FLOAT(gparam->param)) {
		res = filterparam_set(gparam->param, &adj->value);
	} else if (FILTER_PARAM_IS_SAMPLE(gparam->param)) {
		SAMPLE val = adj->value;
		res = filterparam_set(gparam->param, &val);
	} else if (FILTER_PARAM_IS_INT(gparam->param)) {
		int val = adj->value;
		res = filterparam_set(gparam->param, &val);
	}
	if (res == -1) {
		if (FILTER_PARAM_IS_FLOAT(gparam->param))
			gtk_adjustment_set_value(
				gparam->u.adj,
				filterparam_val_float(gparam->param));
		else if (FILTER_PARAM_IS_SAMPLE(gparam->param))
			gtk_adjustment_set_value(
				gparam->u.adj,
				filterparam_val_sample(gparam->param));
		else if (FILTER_PARAM_IS_INT(gparam->param))
			gtk_adjustment_set_value(
				gparam->u.adj,
				filterparam_val_int(gparam->param));
	}
	gparam->updating = 0;

	return res == 0 ? TRUE : FALSE;
}

static gint optionmenu_cb(GtkMenu *menu, GlameParam *gparam)
{
	GtkWidget *act;
	GList *list;
	int res = -1, val;

	if (gparam->updating)
		return TRUE;

	gparam->updating = 1;

	/* Doh - GTK suxx again. */
	act = gtk_menu_get_active(menu);
	DPRINTF("Menu %p - Active %p\n", menu, act);
	list = gtk_container_children(GTK_CONTAINER(menu));
	val = 0;
	while (list) {
		DPRINTF("%i - %p\n", val, list->data);
		if ((GtkWidget *)(list->data) == act)
			break;
		list = g_list_next(list);
		val++;
	}
	if (list) {
		DPRINTF("Setting %s to %i\n", filterparam_label(gparam->param), val);
		res = filterparam_set(gparam->param, &val);
	} else
		DPRINTF("Illegal value for menu\n");
	gparam->updating = 0;

	return res == 0 ? TRUE : FALSE;
}


GtkWidget *glame_param_new(filter_param_t *param)
{
	GlameParam *gparam;
	const char *param_label;
	const char *label;
	char *xml;

	if (!param || FILTER_PARAM_IS_POS(param))
		return NULL;

	gparam = GLAME_PARAM(gtk_type_new(glame_param_get_type()));
	gparam->param = param;

	/* Create actual widget(s).
	 */

	param_label = filterparam_get_property(param, FILTERPARAM_LABEL);
	label = param_label;
	if (!label)
		label = filterparam_label(param);

#ifdef HAVE_LIBGLADE
	if ((xml = filterparam_get_property(param, FILTERPARAM_GLADEXML))) {
		GladeXML *gxml;
		gxml = glade_xml_new_from_memory(xml, strlen(xml), NULL, NULL);
		gparam->label = gtk_label_new(label);
		gparam->widget = glade_xml_get_widget(gxml, "widget");
		gparam->u.widget = gparam->widget;
		if (GTK_IS_OPTION_MENU(gparam->widget)) {
			gtk_option_menu_set_history(GTK_OPTION_MENU(gparam->u.widget), filterparam_val_int(param));
		} else
			DPRINTF("FIXME - unsupported XML widget\n");
	} else
#endif
	if (FILTER_PARAM_IS_INT(param)) {
		gparam->label = gtk_label_new(label);
		gparam->u.adj = GTK_ADJUSTMENT(gtk_adjustment_new(
			filterparam_val_float(param),
			-MAXINT, MAXINT, 1.0, 10.0, 0.0));
		gparam->widget = gtk_spin_button_new(gparam->u.adj, 1, 0);
		gtk_spin_button_set_numeric(GTK_SPIN_BUTTON(gparam->widget),
					    TRUE);
	} else if (filterparam_type(param) == FILTER_PARAMTYPE_TIME_MS) {
		if (param_label)
			gparam->label = gtk_label_new(param_label);
		else {
			char mslabel[256];
			snprintf(mslabel, 255, "%s [ms]", label);
			gparam->label = gtk_label_new(mslabel);
		}
		gparam->u.adj = GTK_ADJUSTMENT(gtk_adjustment_new(
			filterparam_val_float(param),
			0.0, MAXFLOAT, 1.0, 10.0, 0.0));
		gparam->widget = gtk_spin_button_new(gparam->u.adj, 1.0, 0);
		gtk_spin_button_set_numeric(GTK_SPIN_BUTTON(gparam->widget),
					    TRUE);
	} else if (filterparam_type(param) == FILTER_PARAMTYPE_TIME_S) {
		if (param_label)
			gparam->label = gtk_label_new(param_label);
		else {
			char slabel[256];
			snprintf(slabel, 255, "%s [s]", label);
			gparam->label = gtk_label_new(slabel);
		}
		gparam->u.adj = GTK_ADJUSTMENT(gtk_adjustment_new(
			filterparam_val_float(param),
			0.0, MAXFLOAT, 0.1, 1.0, 0.0));
		gparam->widget = gtk_spin_button_new(gparam->u.adj, 0.1, 3);
		gtk_spin_button_set_numeric(GTK_SPIN_BUTTON(gparam->widget),
					    TRUE);
	} else if (filterparam_type(param) == FILTER_PARAMTYPE_POSITION) {
		gparam->label = gtk_label_new(label);
		gparam->u.adj = GTK_ADJUSTMENT(gtk_adjustment_new(
			filterparam_val_float(param),
			-M_PI, M_PI, M_PI/16, M_PI/4, 0.0));
		gparam->widget = gtk_hscale_new(gparam->u.adj);
		gtk_scale_set_digits(GTK_SCALE(gparam->widget), 2);
		gtk_scale_set_draw_value(GTK_SCALE(gparam->widget), TRUE);
	} else if (FILTER_PARAM_IS_FLOAT(param)) {
		gparam->label = gtk_label_new(label);
		gparam->u.adj = GTK_ADJUSTMENT(gtk_adjustment_new(
			filterparam_val_float(param),
			-MAXFLOAT, MAXFLOAT, 0.1, 10.0, 0.0));
		gparam->widget = gtk_spin_button_new(gparam->u.adj, 0.1, 3);
		gtk_spin_button_set_numeric(GTK_SPIN_BUTTON(gparam->widget),
					    TRUE);
	} else if (FILTER_PARAM_IS_SAMPLE(param)) {
		gparam->label = gtk_label_new(label);
		gparam->u.adj = GTK_ADJUSTMENT(gtk_adjustment_new(
			filterparam_val_sample(param),
			-1.0, 1.0, 0.001, 0.05, 0.0));
		gparam->widget = gtk_hscale_new(gparam->u.adj);
		gtk_scale_set_digits(GTK_SCALE(gparam->widget), 3);
		gtk_scale_set_draw_value(GTK_SCALE(gparam->widget), TRUE);
	} else if (filterparam_type(param) == FILTER_PARAMTYPE_FILENAME) {
		gparam->label = gtk_label_new(label);
		gparam->widget = gnome_file_entry_new("editfilter::param::filename", label);
		gparam->u.edit = GTK_EDITABLE(gnome_entry_gtk_entry(
			GNOME_ENTRY(GNOME_FILE_ENTRY(gparam->widget)->gentry)));
		gtk_entry_set_text(GTK_ENTRY(gparam->u.edit),
				   filterparam_val_string(param));
	} else if (FILTER_PARAM_IS_STRING(param)) {
		gparam->label = gtk_label_new(label);
		gparam->u.edit = GTK_EDITABLE(gtk_entry_new());
		gparam->widget = GTK_WIDGET(gparam->u.edit);
		gtk_entry_set_text(GTK_ENTRY(gparam->u.edit),
				   filterparam_val_string(param));
	} else if (filterparam_type(param) == FILTER_PARAMTYPE_SBUF) {
		char *ctl_vec_string, *num_string;
		char * string_begin,*string_next;
		gfloat (*ctl_points)[2];
		int num_points,i;
		gparam->label = gtk_label_new(label);
		gparam->u.curve = GLAME_CURVE(glame_curve_new());
		gparam->widget = GTK_WIDGET(gparam->u.curve);
		gtk_curve_set_range(GTK_CURVE(gparam->u.curve),
				    0.0, 1.0, -1.0, 1.0);
		gtk_widget_set_usize(GTK_WIDGET(gparam->u.curve), 200, 200);
		if ((num_string = filterparam_get_property(param, "curve-control-points"))
		    && (ctl_vec_string = filterparam_get_property(param, "curve-control-points-data"))) {
			num_points = atoi(num_string);
			fprintf(stderr,"num: %s %d\n%s\n",num_string,num_points,ctl_vec_string);
			ctl_points = malloc(num_points*sizeof(gfloat[2]));
			string_begin = ctl_vec_string;
			for(i=0; i<num_points; i++){
				ctl_points[i][0] = strtod(string_begin,&string_next);
				string_begin=string_next;
				ctl_points[i][1] = strtod(string_begin,&string_next);
				string_begin=string_next;
				/*sscanf(ctl_vec_string,"%f %f",&(ctl_points[i][0]),&(ctl_points[i][1]));*/
				/*fprintf(stderr,"set_param: %f %f\n",ctl_points[i][0],ctl_points[i][1]);*/
			}
			glame_curve_set_control_vector(
				gparam->u.curve, num_points, ctl_points);
		}
	} else if (FILTER_PARAM_IS_BUF(param)) {
		DPRINTF("FIXME! - buf params not supported\n");
	} else
		DPRINTF("FIXME! - unsupported param type\n");

	/* Build the hbox, connect to the entry. */
	gtk_box_pack_start(GTK_BOX(gparam), gparam->label, FALSE, TRUE, 10);
	gtk_box_pack_start(GTK_BOX(gparam), gparam->widget, TRUE, TRUE, 10);
	if (GTK_IS_ADJUSTMENT(gparam->u.widget))
		gtk_signal_connect(GTK_OBJECT(gparam->u.adj), "value_changed",
				   (GtkSignalFunc)adjustment_cb, gparam);
	else if (GTK_IS_EDITABLE(gparam->u.widget))
		gtk_signal_connect(GTK_OBJECT(gparam->u.edit), "changed",
				   (GtkSignalFunc)editable_cb, gparam);
	else if (GLAME_IS_CURVE(gparam->u.widget))
		gtk_signal_connect(GTK_OBJECT(gparam->u.curve),
				   "curve_changed",
				   (GtkSignalFunc)curve_cb, gparam);
	else if (GTK_IS_OPTION_MENU(gparam->u.widget))
		gtk_signal_connect(GTK_OBJECT(gtk_option_menu_get_menu(GTK_OPTION_MENU(gparam->u.widget))),
				   "selection_done",
				   (GtkSignalFunc)optionmenu_cb, gparam);
	else
		DPRINTF("FIXME - unsupported widget type\n");

	/* Register handlers for backend param change and deletion. */
	gparam->fhandler = glsig_add_handler(
		filterparam_emitter(param),
		GLSIG_PARAM_CHANGED|GLSIG_PARAM_DELETED, handle_param, gparam);
	glsig_dont_copy_handler(gparam->fhandler);

	return GTK_WIDGET(gparam);
}