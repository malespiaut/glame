#ifndef __GLAME_PARAM_H__
#define __GLAME_PARAM_H__

/*
 * glame_param.h
 *
 * $Id: glame_param.h,v 1.5 2004/10/23 13:09:26 richi Exp $
 *
 * Copyright (C) 2001, 2002 Richard Guenther
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

#include <gtk/gtk.h>
#include "glame_curve.h"
#include "filter.h"
#include "glsignal.h"


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


typedef struct _GlameParam       GlameParam;
typedef struct _GlameParamClass  GlameParamClass;

#define GLAME_TYPE_PARAM	    (glame_param_get_type ())
#define GLAME_PARAM(obj)            (GTK_CHECK_CAST ((obj), GLAME_TYPE_PARAM, GlameParam))
#define GLAME_PARAM_CLASS(klass)    (GTK_CHECK_CLASS_CAST ((klass), GLAME_TYPE_PARAM, GlameParamClass))
#define GLAME_IS_PARAM(obj)         (GTK_CHECK_TYPE ((obj), GLAME_TYPE_PARAM))
#define GLAME_IS_PARAM_CLASS(klass) (GTK_CHECK_CLASS_TYPE ((klass), GLAME_TYPE_PARAM))
#define GLAME_PARAM_GET_CLASS(obj)  ((GlameParamClass *)(((GtkObject *)(obj))->klass))


struct _GlameParam
{
	GtkHBox hbox;

	filter_param_t *param;
	glsig_handler_t *fhandler;

	GtkWidget *label;
	GtkWidget *widget;
	union {
		GtkWidget *widget;
		GtkAdjustment *adj;
		GtkEditable *edit;
		GtkMenuShell *menu;
		GlameCurve *curve;
	} u;

	int updating;
};

struct _GlameParamClass
{
	GtkHBoxClass parent_class;
};


GtkType    glame_param_get_type          (void);
GtkWidget* glame_param_new               (filter_param_t *param);
GtkWidget* glame_param_new_without_label (filter_param_t *param);


#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* __GLAME_PARAM_H__ */
