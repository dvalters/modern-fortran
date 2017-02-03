#include <stdio.h>

float sum_f(float *, int * );

int main(int argc, char *argv[] ) 
{
  float x[4] = { 1.0, 2.0, 3.0, 4.0 };
  int n = 4;
  float res;

  res = sum_f( x, &n );
}

