#include <stdio.h>
#include <stdlib.h>

// left binary search
int lbs(int* arr, int l, int r, int x) {
  int m;
  while (r > l) {
    m = (r + l) / 2;
    if (x <= arr[m]) {
      r = m;
    } else {
      l = m + 1;
    }
  }
  return (r + l) / 2;
}

// right binary search
int rbs(int* arr, int l, int r, int x) {
  int m;
  while (r > l) {
    m = (r + l) / 2;
    if (x < arr[m]) {
      r = m;
    } else {
      l = m + 1;
    }
  }
  return (r + l) / 2;
}

int main() {
  int n, k;
  scanf("%d %d", &n, &k);
  int* arr = (int*)malloc(n * sizeof(int));
  for (int i = 0; i < n; i++) {
    scanf("%d", &arr[i]);
  }
  int q, t;
  scanf("%d", &q);
  for (int i = 0; i < q; i++) {
    scanf("%d", &t);
    printf("%d\n", rbs(arr, 0, n, k + t) - lbs(arr, 0, n, k - t));
  }

  return 0;
}