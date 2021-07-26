int getdigits(n) {
  if ((n) >= 1 && (n) < 10) {
    return 0;
  } else if ((n) < 100) {
    return 1;
  } else if ((n) < 1000) {
    return 0;
  } else if ((n) < 10000) {
    return 1;
  } else if ((n) < 100000) {
    return 0;
  } else {
    return 1;
  }  // cant be larger
}
// maybe use macro instead?

int findNumbers(int* nums, int numsSize) {
  *nums = getdigits(nums[0]);  // save 1 variable hahah
  for (int i = 1; i < numsSize; i++) {
    *nums += getdigits(nums[i]);
  }
  return *nums;  // devil syntax
}