/*
topics:
  - array
  - dynamic programming
  - divide and conquer
difficulty: easy
*/

int maxSubArray(int* nums, int numsSize) {
  int max = INT_MIN;
  for (int i = 0, cur = 0; i < numsSize; ++i) {
    // cummulatively sum the array
    cur += nums[i];

    // if the current sum is max, store
    if (cur > max) max = cur;

    // if the current sum is negative, reset
    if (cur < 0) cur = 0;
  }
  return max;
}