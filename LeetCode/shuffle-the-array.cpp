class Solution {
public:
  vector<int> shuffle(vector<int>& nums, int n) {
    int from, to;
    int hand, lift;
    int assert_i = 1;
    from = 1;
    hand = nums[1];
    while (assert_i < (2*n-1)) {
      // Calculate index
      to = (from < n) ? 2*from : 2*(from-n)+1;
      //printf("TO: %d, FROM: %d, NUMS[TO]: %d\n",to,from,nums[to]);
      if (nums[from] > 0) { // if this index is not place yet       
        lift = abs(nums[to]); // lift the target | the lift is always positive thanks to ABS
        nums[to] = (nums[to] < 0 ) ? -hand : hand; // put the value at hand
        nums[from] = -nums[from]; // hand is placed, we should keep record of it at it's source 
        hand = lift; // now we work with the lifted value, so our hand is what we just lifted
        from = to; // the hand is coming from the index where we just wrote 'to', so in the next iteration this is 'from'
      } else {
        // This means that we have a cycle, and everything in this cycle is placed correctly.
        // As a naive solution, go through the array one by one to find non-negative value, which means that index is not place yet.
        while (assert_i < (2*n-1)) {
          //printf("\tA_i: %d\n", assert_i);
          if (nums[assert_i] > 0) { // this value is not placed yet
            from = assert_i; // remember where it came from
            hand = nums[from]; // take it in your hand
            break;
          }
          assert_i++;
        }
      }
      
    }
    // fix negatives
    for(int i = 1; i<2*n-1; i++) {
      nums[i] = -nums[i];
    }
    return nums;
  }
};

/**
O(n) Time, O(1) Space implementation #1
All values are placed once. O(n) time
The assertion asserts all indexes once. O(n) time
Negatives are fixed at the end O(n) time
*/

// my post: https://leetcode.com/problems/shuffle-the-array/discuss/921585/C%2B%2B-O(1)-Space-O(N)-Time-(Inline-and-Linear)-100-Memory-90-Time