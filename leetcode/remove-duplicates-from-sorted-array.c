int removeDuplicates(int* nums, int numsSize) {
  int i, offset = 0, vals = 0;
  for (i = 1; i < numsSize; i++) {
    if (nums[i] == nums[i - 1]) {
      vals++;
      offset++;
    } else {
      nums[i - offset] = nums[i];
    }
  }
  return numsSize - vals;
}

/*
This is just like remove-element problem, but instead of
"val" in line 4, we use "nums[i-1]". This is due to the fact that
our array is sorted!
*/