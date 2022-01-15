#include <stdio.h>

int main() {
  int N;
  int max, cur, ans = 0;
  scanf("%d", &N);
  scanf("%d", &max);
  for (int i = 1; i < N; ++i) {
    scanf("%d", &cur);
    if (cur > max) {
      ans++;
      max = cur;
    }
  }
  printf("%d", ans);
  return 0;
}
