#include "glame_curve.h"
#include <stdio.h>
#include "util.h"

#define RADIUS          3 

static void glame_curve_class_init(GlameCurveClass* klass)
{
	/* nothing to be done here, no signals... */
}

static void glame_curve_init(GlameCurve* gcurve)
{
	gint old_mask;
	GtkCurve* curve=GTK_CURVE(gcurve);
	
	curve->cursor_type = GDK_TOP_LEFT_ARROW;
	curve->pixmap = NULL;
	curve->curve_type = GTK_CURVE_TYPE_SPLINE;
	curve->height = 0;
	curve->grab_point = -1;
	
	curve->num_points = 0;
	curve->point = 0;
	
	curve->num_ctlpoints = 0;
	curve->ctlpoint = NULL;
	
	curve->min_x = 0.0;
	curve->max_x = 1.0;
	curve->min_y = 0.0;
	curve->max_y = 1.0;
	
	
	
}
 
GtkWidget* glame_curve_new(void)
{
	return GTK_WIDGET( gtk_type_new(glame_curve_get_type()));
}


static int
project (gfloat value, gfloat min, gfloat max, int norm)
{
  return (norm - 1) * ((value - min) / (max - min)) + 0.5;
}


static void 
gtk_curve_interpolate (GtkCurve *c, gint width, gint height)
{
  gfloat *vector;
  int i;

  vector = g_malloc (width * sizeof (vector[0]));

  gtk_curve_get_vector (c, width, vector);

  c->height = height;
  if (c->num_points != width)
    {
      c->num_points = width;
      if (c->point)
        g_free (c->point);
      c->point = g_malloc (c->num_points * sizeof (c->point[0]));
    }

  for (i = 0; i < width; ++i)
    {
      c->point[i].x = RADIUS + i;
      c->point[i].y = RADIUS + height
        - project (vector[i], c->min_y, c->max_y, height);
    }

  g_free (vector);
}


static void glame_curve_redraw(GtkCurve* c)
{
	GtkStateType state;
	GtkStyle *style;
	gint i;
	guint width, height;

	width = GTK_WIDGET(c)->allocation.width - RADIUS * 2;
	height = GTK_WIDGET(c)->allocation.height - RADIUS * 2;

	if (!c->pixmap)
		return;
	
	if (c->height != height || c->num_points != width)
		gtk_curve_interpolate (c, width, height);
	
	state = GTK_STATE_NORMAL;
	if (!GTK_WIDGET_IS_SENSITIVE (GTK_WIDGET (c)))
		state = GTK_STATE_INSENSITIVE;
	
	style = GTK_WIDGET (c)->style;
	
	/* clear the pixmap: */ 
	gtk_paint_flat_box (style, c->pixmap, GTK_STATE_NORMAL, GTK_SHADOW_NONE,
			    NULL, GTK_WIDGET(c), "curve_bg",
			    0, 0, width + RADIUS * 2, height + RADIUS * 2);
	/* draw the grid lines: (XXX make more meaningful) */ 
	for (i = 0; i < 5; i++)
		{
			gdk_draw_line (c->pixmap, style->dark_gc[state],
				       RADIUS, i * (height / 4.0) + RADIUS,
				       width + RADIUS, i * (height / 4.0) + RADIUS);
			gdk_draw_line (c->pixmap, style->dark_gc[state],
				       i * (width / 4.0) + RADIUS, RADIUS,
				       i * (width / 4.0) + RADIUS, height + RADIUS);
		}
	
	gdk_draw_points (c->pixmap, style->fg_gc[state], c->point, c->num_points);
	if (c->curve_type != GTK_CURVE_TYPE_FREE)
		for (i = 0; i < c->num_ctlpoints; ++i)
			{
				gint x, y;
				
				if (c->ctlpoint[i][0] < c->min_x)
					continue;
				x = project (c->ctlpoint[i][0], c->min_x, c->max_x,
					     width);
				y = height -
					project (c->ctlpoint[i][1], c->min_y, c->max_y,
						 height);
				
				/* draw a bullet: */ 
				gdk_draw_arc (c->pixmap, style->fg_gc[state], TRUE, x, y,
					      RADIUS * 2, RADIUS*2, 0, 360*64);
			}
	gdk_draw_pixmap (GTK_WIDGET (c)->window, style->fg_gc[state], c->pixmap,
			 0, 0, 0, 0, width + RADIUS * 2, height + RADIUS * 2);
	
}




GtkType glame_curve_get_type(void)
{
	static guint curve_type = 0;
	
	if(!curve_type){
		GtkTypeInfo curve_info = {
			"GlameCurve",
			sizeof(GlameCurve),
			sizeof(GlameCurveClass),
			glame_curve_class_init,
			glame_curve_init,
			NULL,
			NULL
		};
		
		curve_type = gtk_type_unique(gtk_curve_get_type(),&curve_info);
	}
	
	return curve_type;
}


    
void glame_curve_set_control_vector(GlameCurve* curve,
				    int num_points,
				    gfloat (*ctl_points)[2])
{
	int width,height;
	GtkCurve* gCurve = GTK_CURVE(curve);
	for(width=0;width<num_points;width++){
		fprintf(stderr,"set: %f %f\n",ctl_points[width][0],ctl_points[width][1]);
	}

	gCurve->num_ctlpoints = num_points;
	if(gCurve->ctlpoint)
		g_free(gCurve->ctlpoint);
	gCurve->ctlpoint = malloc(gCurve->num_ctlpoints*sizeof(gfloat[2]));
	memcpy(gCurve->ctlpoint,ctl_points,gCurve->num_ctlpoints*sizeof(gfloat[2]));
	if(gCurve->num_points){
		g_free(gCurve->point);
		gCurve->point=NULL;
		gCurve->num_points=0;
	}
	for(width=0;width<num_points;width++){
		fprintf(stderr,"set: %f %f\n",ctl_points[width][0],ctl_points[width][1]);
	}
	/* FIXME */
	//glame_curve_redraw(gCurve);
		gtk_curve_set_curve_type(gCurve,GTK_CURVE_TYPE_SPLINE);//gCurve->curve_type);
}

void glame_curve_get_control_vector(GlameCurve* curve,
				    int* num_points,
				    gfloat ***ctl_points)
{
	int width;
	gfloat (*foo)[2];
	GtkCurve* gCurve = GTK_CURVE(curve);
	*num_points = gCurve->num_ctlpoints;
	DPRINTF("Controlpoints: %d\n",*num_points);
	*ctl_points = malloc(gCurve->num_ctlpoints*sizeof(*foo));
	foo = *ctl_points;
	memcpy(*ctl_points,gCurve->ctlpoint,gCurve->num_ctlpoints*sizeof(*foo));
	for(width=0;width<*num_points;width++){
		fprintf(stderr,"get: %f %f\n",foo[width][0],foo[width][1]);
	}
}


