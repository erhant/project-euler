char* longestCommonPrefix(char** strs, int strsSize) {
  unsigned char c, si, i;
  for (i = 0; strs[0][i] != '\0'; ++i) {
    c = strs[0][i];
    for (si = 1; si < strsSize; si++) {
      if (c != strs[si][i]) {
        strs[0][i] = '\0';
        return strs[0];
      }
    }
  }
  return strs[0];
}

// my post: https://leetcode.com/problems/longest-common-prefix/discuss/1364014/C-or-100-faster-0ms-simple