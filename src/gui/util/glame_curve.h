#ifndef _GLAME_CURVE
#define _GLAME_CURVE

/*
 * glame_curve.h
 *
 * $Id: glame_curve.h,v 1.2 2001/06/22 08:49:33 richi Exp $
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

#include <gtk/gtk.h>


typedef struct _GlameCurve GlameCurve;
typedef struct _GlameCurveClass GlameCurveClass;


#define GLAME_TYPE_CURVE                  (glame_curve_get_type ())
#define GLAME_CURVE(obj)                  (GTK_CHECK_CAST ((obj), GLAME_TYPE_CURVE, GlameCurve))
#define GLAME_CURVE_CLASS(klass)          (GTK_CHECK_CLASS_CAST ((klass), GLAME_TYPE_CURVE, GlameCurveClass))
#define GLAME_IS_CURVE(obj)               (GTK_CHECK_TYPE ((obj), GLAME_TYPE_CURVE))
#define GLAME_IS_CURVE_CLASS(klass)       (GTK_CHECK_CLASS_TYPE ((klass), GLAME_TYPE_CURVE))


struct _GlameCurve {
  GtkCurve curve;
  
};

struct _GlameCurveClass {
  GtkCurveClass parent_class;
};



void glame_curve_get_control_vector(GlameCurve* curve,
				    int * num_points,
				    gfloat (*(*ctl_points))[2]);


void glame_curve_set_control_vector(GlameCurve* curve,
				    int num_points,
				    gfloat (*ctl_points)[2]);

GtkType glame_curve_get_type(void);
GtkWidget* glame_curve_new(void);


#endif
