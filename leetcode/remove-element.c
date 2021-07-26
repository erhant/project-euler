int removeElement(int* nums, int numsSize, int val) {
  int i, offset = 0, vals = 0;
  for (i = 0; i < numsSize; i++) {
    if (nums[i] == val) {
      vals++;
      offset++;
    } else {
      nums[i - offset] = nums[i];
    }
  }
  return numsSize - vals;
}