#import <stdio.h>

const int SIZE = 7;
int  cnt;
int  tenth;
char c[7];
int  data;
int  flag;
int  opdv;

void opd(char s, int k, char j) {
  int t = 0;
  if (j == '0') {
    t = 0;
  } else if (j == '1') {
    t = 1;
  } else if (j == '2') {
    t = 2;
  } else if (j == '3') {
    t = 3;
  } else if (j == '4') {
    t = 4;
  } else if (j == '5') {
    t = 5;
  } else if (j == '6') {
    t = 6;
  } else if (j == '7') {
    t = 7;
  } else if (j == '8') {
    t = 8;
  } else if (j == '9') {
    t = 9;
  } else {
    printf("%c is not a number\n", j);
    flag = 1;
  }

  if (!flag) {
    if (s == '+') {
      t *= k;
    } else {
      t *= -k;
    }
    opdv += t;
  }
}

void flush() {
  data = 0;
  c[0] = getchar();
  while (c[0] != '\n') {
    c[0] = getchar();
  }
}

void getdata() {
  if (cnt < SIZE) {
    c[cnt] = getchar();
    cnt++;
  } else {
    printf("Number too Big\n");
    flush();
  }
}

void reset(int w, int x, int y, int z) {
  int k;
  for (k = 0; k < SIZE; k++) {
    c[k] = 0;
  }
  data = w;
  opdv = x;
  cnt = y;
  flag = z;
}

int main() {
  reset(1, 0, 0, 0);
  getdata();
  while (c[0] != '@') {
    if (c[0] == '+' || c[0] == '-') {
      getdata();
    } else {
      c[1] = c[0];
      c[0] = '+';
      cnt++;
    }
    while (data) {
      if (c[cnt - 1] == '\n') {
	data = 0;
	tenth = 1;
	cnt = cnt - 2;

	while (!flag && cnt != 0) {
	  opd(c[0], tenth, c[cnt]);
	  cnt--;
	  tenth *= 10;
	}
	if (!flag) {
	  printf("Operand is %d\n", opdv);
	}
      } else {
	getdata();
      }
    }
    reset(1, 0, 0, 0);
    getdata();
  }

  return 0;
}
