// usr/bin/gcc "$0" && exec ./a.out "$@"
#include <stdio.h>

int X, Y;

int climb(int n) {
  if (n == 0) return 1;
  if (n < 0) return 0;
  return climb(n - X) + climb(n - Y);
}

int main() {
  int N;
  scanf("%d", &N);
  scanf("%d %d", &X, &Y);
  printf("%d", climb(N));
  return 0;
}
