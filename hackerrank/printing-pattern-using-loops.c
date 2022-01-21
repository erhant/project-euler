#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void line(int l, int n, int d) {
  for (int i = 0; i < d; i++) {
    printf("%d ", n);
    n--;
  }
  for (int i = d; i < l - 1 - d; i++) {
    printf("%d ", n);
  }
  for (int i = l - 1 - d; i < l; i++) {
    printf("%d ", n);
    n++;
  }
  printf("\n");
}

int main() {
  int n;
  scanf("%d", &n);

  const int l = n + n - 1;

  int d = 0;
  for (int i = 0; i < n - 1; i++) {
    line(l, n, d);
    d++;
  }
  line(l, n, d);
  for (int i = 0; i < n - 1; i++) {
    d--;
    line(l, n, d);
  }

  return 0;
}