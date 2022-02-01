/*
topics:
  - array
  - math
  - negative painting
difficulty: easy
*/

int missingNumber(int* nums, int n) {
  int i, src;

  // paint the indexed numbers negative, add 1 to everything to handle 0
  for (i = 0; i < n; ++i) {
    src = nums[i];                     // source index is nums[i]
    src = src < 0 ? -(src + 1) : src;  // this may be painted before, so neutralize it

    // paint the destination
    if (src != n) {
      nums[src] = -(nums[src] + 1);
    }
  }

  // loop until the end or first non-negative
  for (i = 0; i < n && nums[i] < 0; ++i)
    ;

  // return it's index
  return i;
}