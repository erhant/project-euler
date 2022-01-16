#include <stdio.h>

int main() {
  int N, K;
  char c;
  scanf("%d %d\n", &N, &K);
  int H = 0, S = 0;
  for (int i = 0; i < K; i++) {
    scanf("%c", &c);
    if (c == 'H')
      H++;
    else
      S++;
  }

  // we have scores of H and S now
  // the score difference must be larger than remaining rounds
  const int diff = H > S ? H - S : S - H;
  if (diff > N - K) {
    printf(H > S ? "Harun" : "Sami");
  } else {
    printf("Cilek");
  }

  return 0;
}