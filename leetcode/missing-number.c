/*
topics:
  - array
  - math
  - negative painting
difficulty: easy
*/

int missingNumber(int* nums, int n) {
  int i, src;

  // paint the indexed numbers negative
  // +1 because 0 can't be painted
  for (int i = 0; i < n; ++i) {
    src = nums[i];                     // source index is nums[i]
    src = src < 0 ? -(src + 1) : src;  // this may be painted before, so neutralize it
    // paint the destination
    if (src != n) {
      nums[src] = -(nums[src] + 1);
    }
  }

  // find the first non-painted value
  for (int i = 0; i < n; ++i) {
    if (nums[i] >= 0) {
      return i;
    }
  }
  return n;
}