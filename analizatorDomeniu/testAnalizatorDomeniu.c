int x;
char y;
double z;
double p[100];

struct S1{
    char x;
	int i;
	double d[2];
	};
struct S1 p1;
struct S1 vp[10];


double sum(double x[5],int n){
	double r;
	int i;
	r=0;
	for(i=0;i<n;i=i+1){
		double n;
		n=x[i];
		r=r+n;
		}
	return r;
	}
