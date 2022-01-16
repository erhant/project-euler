#include <stdio.h>

long long consumeDigits(long long N) {
  long long mul = 1, d;
  while (N > 0) {
    d = N % 10;
    if (d > 1) mul *= d;
    N /= 10;
  }
  return (mul < 10) ? mul : consumeDigits(mul);
}

int main() {
  long long N;
  scanf("%lld", &N);
  printf("%lld", consumeDigits(N));
  return 0;
}