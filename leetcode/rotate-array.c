/*
topics:
  - math
  - array
  - two pointers
difficulty: medium
*/

void rotate(int* nums, int n, int k) {
  k = k % n;        // n rotations = 0 rotations
  int toPlace = n;  // we must place n numbers
  int i, j, hand, lift;
  for (int c_i = 0; toPlace; c_i++) {
    i = c_i;         // cycle starts here
    hand = nums[i];  // take current number in your hand
    do {
      // find destination
      j = (i + k) % n;
      // place the number to its destination
      lift = nums[j];  // lift the number there
      nums[j] = hand;  // place the one in your hand
      hand = lift;     // take the lifted one to your hand
      // move on to next position
      i = j;
      // this number is placed now, 1 less to go
      toPlace--;
    } while (i != c_i);  // keep doing this until you come back to the start
  }
}

/*
I wonder if it is possible to know how many cycles you may need, and for each cycle how many rotations?
*/