#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <fixp.h>

int main(int argc, char **argv)
{

	volatile float f=1.0;
	volatile fixp_t x;
	int i=100000000;

	fixp_set_bias(16);

	float2fixp(f);

	if(argc!=3) {
		fprintf(stderr, "Usage: %s ( f | x ) ( + | * )\n", argv[0]);
		exit(1);
	}

	switch(argv[1][0]) {
		case 'f':
			printf("Timing float ");
			if(argv[2][0]=='+') {
				printf("add\n");
				while(i--) f=f+f;
			} else {
				printf("mul\n");
				while(i--) f=f+f;
			}
			break;
		case 'x':
			printf("Timing fixp ");
			if(argv[2][0]=='+') {
				printf("add\n");
				while(i--) x=fixp_adw(x, x);
			} else {
				printf("mul\n");
				while(i--) x=fixp_muw(x, x);
			}
			break;
		default:
			fprintf(stderr, "Must specify one of f or x.\n");
			exit(1);
	}
	exit(0);
}
	
