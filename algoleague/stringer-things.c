#include <stdio.h>

int main() {
  int N;
  char c;
  scanf("%d", &N);
  scanf(" %c", &c);  // read with a space considered
  printf("%c", c);   // first char is printed regardless
  N--;
  char prev = c;
  for (; N > 0; N--) {
    scanf("%c", &c);
    if (c != prev) {
      prev = c;
      printf("%c", c);
    }
  }
  return 0;
}