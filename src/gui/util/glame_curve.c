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


