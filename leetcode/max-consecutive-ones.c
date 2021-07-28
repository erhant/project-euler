/*
topics:
  - array
difficulty: easy
*/

int findMaxConsecutiveOnes(int* nums, int n) {
  int curOnes = 0;
  int maxOnes = 0;
  for (int i = 0; i < n; i++) {
    if (nums[i] == 1) {
      curOnes++;
    } else {
      if (curOnes > maxOnes) {
        maxOnes = curOnes;
      }
      curOnes = 0;
    }
  }
  return (curOnes > maxOnes) ? curOnes : maxOnes;
}