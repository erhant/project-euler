int firstMissingPositive(int* nums, int numsSize) {
  // 1 - Paint n> nums and non-positive as n+1
  for (int i = 0; i < numsSize; i++) {
    if (nums[i] > numsSize || nums[i] <= 0) {
      nums[i] = numsSize + 1;
    }
  }
  // 2 - Index Magic!
  // treat the number as an index (1-based), and paint the numbers with negative.
  int num;
  for (int i = 0; i < numsSize; i++) {
    num = (nums[i] < 0) ? -nums[i] : nums[i];  // todo: refactor
    if (num <= numsSize) {
      nums[num - 1] =
          (nums[num - 1] > 0 ? -nums[num - 1] : nums[num - 1]);  // to avoid duplicate numbers overwriting eachother
    }
  }
  // 3 - Final Search
  for (int i = 0; i < numsSize; i++) {
    if (nums[i] > 0) {
      return i + 1;
    }
  }
  return numsSize + 1;
}
/*
n = 5
1, 1, 8, 9, 11

// 1 - Paint n> numbers and non-positives as n+1
1, 1, 6, 6, 6

// 2 - Index Magic
-1 1  6  6  6

// 3 - Search first positive index
x  ^  x  x  x

= 1+1 = 2
*/