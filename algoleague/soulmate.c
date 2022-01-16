#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

// if \exists i, j s.t.p | a[i] + a[j] ^ i /= j returns true
bool hasSoulmate(int* nums, int N, int P) {
  bool* required = (bool*)calloc(sizeof(bool), P);
  for (int i = 0; i < N; i++) {
    if (required[nums[i]]) {
      // another number has the required remainder
      free(required);
      return true;
    } else {
      // store this remainder
      required[(P - nums[i]) % P] = true;
    }
  }
  free(required);
  return false;
}

int main() {
  int N, P;
  scanf("%d %d\n", &N, &P);
  int* nums = (int*)malloc(sizeof(int) * N);
  for (int i = 0; i < N; ++i) {
    scanf("%d", &nums[i]);
    nums[i] = ((nums[i] % P) + P) % P;  // modulo is distributive
  }
  printf(hasSoulmate(nums, N, P) ? "Yes" : "No");

  free(nums);
  return 0;
}
