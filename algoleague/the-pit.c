#include <stdio.h>

int N;
char STR[20] = {'\0'};

void printBinary(short idx) {
  if (idx == N) {
    printf("%s\n", STR);
  } else {
    STR[idx] = '0';
    printBinary(idx + 1);
    STR[idx] = '1';
    printBinary(idx + 1);
  }
}

int main() {
  scanf("%d", &N);
  printBinary(0);
  return 0;
}