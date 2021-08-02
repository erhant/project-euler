/*
topics:
  - array
  - sorting
  - two pointers
  - important
difficulty: easy
*/

void merge(int* A, int nums1Size, int m, int* B, int nums2Size, int n) {
  m--;
  n--;
  for (int l = nums1Size - 1; l >= 0; --l) {
    if (m < 0) {
      A[l] = B[n--];
    } else if (n < 0) {
      A[l] = A[m--];
    } else {
      if (A[m] > B[n]) {
        A[l] = A[m--];
      } else {
        A[l] = B[n--];
      }
    }
  }
}

/*
Index starting points:

     m     l
[1 2 3 0 0 0]

     n
[2 5 6]

*/
