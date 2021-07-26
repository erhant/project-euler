class Solution {
 public:
  int hammingWeight(uint32_t n) {
    int sum = 0, i = 0;
    for (; i < 32 && n > 0; i++) {
      sum += !((n & 1) == 0);
      n = n >> 1;
    }
    return sum;
  }
};