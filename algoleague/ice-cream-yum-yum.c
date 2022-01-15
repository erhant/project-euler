#include <stdio.h>

int main() {
  int N;
  int sum = 0;
  int min = 10000;
  int A_i;
  scanf("%d", &N);
  for (int i = 0; i < N; ++i) {
    scanf("%d", &A_i);
    sum += A_i;
    if (A_i < min) {
      min = A_i;
    }
  }
  printf("%d", sum - min);
  return 0;
}