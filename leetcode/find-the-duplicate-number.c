/*
topics:
  - negative painting
  - array
  - two pointers
  - binary search
  - bit manipulation
difficulty: medium
*/

// using negative painting
int findDuplicate(int* nums, int numsSize) {
  int i = 0;
  while (nums[i] > 0) {
    nums[i] = -nums[i];
    i = -nums[i];
  }
  return i;
}

// there are lots of other methods to solve this problem too, one including cycle detection