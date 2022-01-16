#include <stdio.h>

int arr[10000];

void merge(int l, int m, int r) {
  int i, j, k;
  int n1 = m - l + 1;
  int n2 = r - m;

  // transfer
  int L[n1], R[n2];
  for (i = 0; i < n1; i++) L[i] = arr[l + i];
  for (j = 0; j < n2; j++) R[j] = arr[m + 1 + j];

  // merge
  i = 0;
  j = 0;
  k = l;
  while (i < n1 && j < n2) {
    if (L[i] <= R[j])
      arr[k++] = L[i++];
    else
      arr[k++] = R[j++];
  }

  // copy remaining
  while (i < n1) arr[k++] = L[i++];
  while (j < n2) arr[k++] = R[j++];
}

int mergeSort(int l, int r) {
  if (l < r) {
    int m = l + (r - l) / 2;  // avoid overflow
    int cl = mergeSort(l, m);
    int cr = mergeSort(m + 1, r);
    merge(l, m, r);
    return cl + cr + 1;
  } else {
    return 0;
  }
}

int main() {
  int N;
  scanf("%d", &N);
  for (int i = 0; i < N; i++) {
    scanf("%d", &arr[i]);
  }
  int cnt = mergeSort(0, N - 1);
  for (int i = 0; i < N; i++) {
    printf("%d ", arr[i]);
  }
  printf("\n%d", cnt);
  return 0;
}