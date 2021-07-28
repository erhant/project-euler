/*
topics:
  - math
difficulty: easy
*/

int reverse(int x) {
  int rev = 0, last;
  while (x != 0) {
    // Pop
    last = x % 10;  // get the last digit from right
    x /= 10;        // move digits to the right
    // Check overflow
    if ((rev > INT_MAX / 10 || (rev == INT_MAX / 10 && last > 7)) ||
        (rev < INT_MIN / 10 || (rev == INT_MIN / 10 && last < -8)))
      return 0;
    // Push
    rev *= 10;    // move digits to the left
    rev += last;  // add the last digit to the right
  }
  return rev;
}
// Integer limits:
// Min: -2147483648
// Max: 2147483647