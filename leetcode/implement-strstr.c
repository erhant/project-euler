/*
topics:
  - two pointers
  - string
  - string matching
  - knuth-morris-pratt
  - rabin-karp
difficulty: easy
*/

// Knuth-Morris-Pratt Pattern Search Algorithm
int strStr(char* s, char* w) {
  // edge cases
  if (!w[0]) return 0;   // empty needle
  if (!s[0]) return -1;  // empty haystack
  // precompute table for KMP
  const int len_w = strlen(w);
  int* T = malloc((1 + len_w) * sizeof(int));
  int pos = 1, cnd = 0;
  T[0] = -1;
  while (pos < len_w) {
    if (w[pos] == w[cnd]) {
      T[pos] = T[cnd];
    } else {
      T[pos] = cnd;
      while (cnd >= 0 && w[pos] != w[cnd]) {
        cnd = T[cnd];
      }
    }
    pos++;
    cnd++;
  }
  T[pos] = cnd;
  // start searching
  int k = 0, j = 0;
  while (s[j]) {
    if (s[j] == w[k]) {
      j++;
      k++;
      if (w[k] == '\0') {
        return j - k;
      }
    } else {
      k = T[k];
      if (k < 0) {
        j++;
        k++;
      }
    }
  }
  free(T);
  return -1;
}