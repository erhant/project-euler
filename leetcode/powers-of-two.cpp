class Solution {
 public:
  bool isPowerOfTwo(int n) {
    if (n <= 0) return false;
    unsigned int m = 1;
    for (int i = 1; i <= sizeof(int) * 8; i++) {
      // printf("m: %d, n: %d\n", m, n);
      if ((m ^ n) == 0) {
        return true;
      }
      m = m << 1;
    }
    return false;
  }
};