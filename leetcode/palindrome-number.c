/*
topics:
  - math
difficulty: easy
*/

// Auxillary to get the number of digits
int numDigits(int n) {
  if (n < 100000)
    if (n < 100)
      if (n < 10)
        return 1;
      else
        return 2;
    else if (n < 1000)
      return 3;
    else if (n < 10000)
      return 4;
    else
      return 5;
  else if (n < 10000000)
    if (n < 1000000)
      return 6;
    else
      return 7;
  else if (n < 100000000)
    return 8;
  else if (n < 1000000000)
    return 9;
  else
    return 10;  // cant be more
}

bool isPalindrome(int x) {
  if (x < 0) return false;
  if (x < 10) return true;

  int m = pow(10, numDigits(x) - 1);
  int div = x / m;

  // printf("%d %d %d %d\n", div, x % 10, m, x);
  while (m > 1 && ((div = x / m) == x % 10)) {
    x -= m * div;  // get rid of leftmost digit
    x /= 10;       // get rid of rightmost digit
    m /= 100;      // adjust the divider
    // printf("%d %d %d %d\n", div, x % 10, m, x);
  }

  return m <= 1;
}