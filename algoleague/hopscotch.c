#include <stdbool.h>
#include <stdio.h>

int main() {
  long long N, K, S = 0;
  scanf("%lld %lld", &N, &K);

  K--;
  S++;                                        // enter from outside into the hopscotch
  K = K % (2 * (N - 1));                      // will cycle back
  K = (K > (N - 1)) ? (2 * (N - 1) - K) : K;  // turning around
  S += K;                                     // final step
  // find the hop value
  const long long H = 3 * ((S + 1) / 2);
  if (S & 1) {
    // odd
    printf("%lld", H - 2);
  } else {
    // even
    printf("%lld %lld", H - 1, H);
  }
  return 0;
}