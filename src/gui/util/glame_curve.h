#ifndef _GLAME_CURVE
#define _GLAME_CURVE

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
				    gfloat ***ctl_points);


void glame_curve_set_control_vector(GlameCurve* curve,
				    int num_points,
				    gfloat (*ctl_points)[2]);

GtkType glame_curve_get_type(void);
GtkWidget* glame_curve_new(void);


#endif


