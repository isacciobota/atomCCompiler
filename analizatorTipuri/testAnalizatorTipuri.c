struct S{
	int n;
	char text[16];
	};

struct S a;
struct S v[10];

void f(char text[],int i,char ch){
	text[i]=ch;
	struct S b;
	b = v[1];
	}

int h(int x,int y){
    int k;
	if(x>0&&x<y){
		f(v[x].text,y,'#');
		return 1;
		}
	return 0;
	}

