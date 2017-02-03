/* sum_c.c */

float sum_c( float *x, int n)
{
  float sum = 0.0f;
  int i;
  
  for( i=0; i<n; i++)
  {
    sum = sum + x[i];
  }
  return sum;
}

