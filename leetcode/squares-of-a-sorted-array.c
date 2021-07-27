int* sortedSquares(int* nums, int n, int* returnSize) {
  int* ans = malloc(n * sizeof(int));
  *returnSize = n;
  int i = 0, j = 0, k = 0;

  // find the point between negative and positive
  while (j < n && nums[j] < 0) j++;
  i = j - 1;

  // merge
  while (i >= 0 && j < n) {
    if (-nums[i] < nums[j]) {
      ans[k++] = nums[i] * nums[i];
      i--;
    } else {
      ans[k++] = nums[j] * nums[j];
      j++;
    }
  }
  for (; i >= 0; i--) ans[k++] = nums[i] * nums[i];
  for (; j < n; j++) ans[k++] = nums[j] * nums[j];

  return ans;
}