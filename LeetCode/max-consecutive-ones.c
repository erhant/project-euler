int findMaxConsecutiveOnes(int* nums, int numsSize){
  int i = 0;
  int curOnes = 0;
  int maxOnes = 0;
  for (; i<numsSize; i++) {
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