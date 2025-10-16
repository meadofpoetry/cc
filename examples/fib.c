
int main(void)
{
  int a = 0;
  int b = 1;

  for (int n = 12; n > 0; n = n - 1) {
    int tmp = b;
    b = a + b;
    a = tmp;
  }
 
  return a;
}
