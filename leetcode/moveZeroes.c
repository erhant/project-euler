void moveZeroes(int* nums, int numsSize) {
  int i = 0, offset = 0, zeros = 0;
  for (; i < numsSize; i++) {
    if (nums[i] == 0) {
      zeros++;
      offset++;
    } else {
      nums[i - offset] = nums[i];
    }
  }
  memset(&nums[numsSize - zeros], 0, zeros * sizeof(int));
}