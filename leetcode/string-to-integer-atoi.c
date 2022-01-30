/*
topics:
  - string
difficulty: medium
*/

int myAtoi(char* s) {
  bool isNegative = false;
  long double n = 0;  // why is this better than long long int?
  // consume leading whitespace
  while (*s == ' ') s++;
  // consume sign
  if (*s == '-') {
    isNegative = true;
    s++;
  } else if (*s == '+') {
    s++;
  }
  // read digits
  while (*s && (*s) - '0' >= 0 && (*s) - '0' <= 9) {
    n = 10 * n + ((*s) - '0');
    s++;
  }
  n = isNegative ? -n : n;
  // boundary checks
  if (n > INT_MAX) return INT_MAX;
  if (n < INT_MIN)
    return INT_MIN;
  else
    return n;
}