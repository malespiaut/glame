#include <fixp.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

void usage(char *name)
{

	fprintf(stderr, "%s: num1 ( + | - | * | / ) num2\n", name);
	exit(1);
}

float fadd_h(float x, float y)
{
	return x+y;
}

float fsub_h(float x, float y)
{
	return x-y;
}

float fmul_h(float x, float y)
{
	return x*y;
}

float fdiv_h(float x, float y)
{
	return x/y;
}

float nosym(float x, float y)
{
	fprintf(stderr, "No such symbol.\n");
	exit(1);
	return 0.0; /* dummy */
}

int main(int argc, char **argv)
{
	int i=-1;
	float f1, f2, fres;
	fixp_t x1, x2, xres;
	char symbols[4]={ '+', '-', '*', '/' };
	float (*ftab[5])(float, float)={ fadd_h, fsub_h, fmul_h, fdiv_h, nosym };
	fixp_t (*xtab[4])(fixp_t, fixp_t)={ fixp_adw, fixp_suw, fixp_muw, fixp_div };
	
	if(argc != 4)
		usage(argv[0]);

	fixp_set_bias(16);

	f1 = atof(argv[1]);
	f2 = atof(argv[3]);
	x1 = float2fixp(f1);
	x2 = float2fixp(f2);

	while(++i<4)
		if(symbols[i]==argv[2][0])
			break;

	fres=(*ftab[i])(f1, f2);
	xres=(*xtab[i])(x1, x2);
	
	printf("floating point: %f %c %f = %f\n", f1, argv[2][0], f2, fres);
	f1 = fixp2float(x1);
	f2 = fixp2float(x2);
	fres = fixp2float(xres);
	printf("fixed point:    %f %c %f = %f\n", f1, argv[2][0], f2, fres);
	printf("raw:            0x%x %c 0x%x = 0x%x\n", x1, argv[2][0], x2, xres);
	return 0;
}
