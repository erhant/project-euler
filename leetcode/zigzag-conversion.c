/*
topics:
  - string
difficulty: medium
*/

char* convert(char* s, int m) {
  if (m == 1) return s;
  const int n = strlen(s);
  char* ans = malloc(n * sizeof(char));
  int k = 0, i = 0, x = (m - 1) << 1;
  int adder[2] = {x - 2, 2};
  bool a = 0;
  // first step
  i = 0;
  while (i < n) {
    ans[k++] = s[i];
    i += x;
  }
  // intermediate
  for (int row = 1; row < m - 1; ++row) {
    i = row;
    while (i < n) {
      ans[k++] = s[i];
      i += adder[a];  // p or q
      a = !a;         // 0 -> 1 or 1 -> 0
    }
    a = 0;
    adder[0] -= 2;
    adder[1] += 2;
  }
  // last step
  i = m - 1;
  while (i < n) {
    ans[k++] = s[i];
    i += x;
  }
  memcpy(s, ans, n * sizeof(char));
  free(ans);
  return s;
}

/*
The additions for each row
m = 1
  just return

m = 2
  (0) + 2 + 0
  (1) + 0 + 2

m = 3
  (0) + 4 + 0
  (1) + 2 + 2
  (2) + 0 + 4

m = 4
  (0) + 6 + 0
  (1) + 4 + 2
  (2) + 2 + 4
  (3) + 0 + 6

...

*/