/*
topics:
  - string
  - hashmap
  - sliding window
difficulty: medium
*/

int lengthOfLongestSubstring(char* s) {
  int SEEN_LAST_AT[256];
  int ans = 0, left = 0;
  // ugly but cute memset
  memset(SEEN_LAST_AT, -1, 256 * sizeof(int));
  for (int right = 0; s[right]; right++) {
    // set the left to one after the index of this character
    left = fmax(left, SEEN_LAST_AT[s[right]] + 1);
    // update the current subsequence length if greater
    ans = fmax(ans, right - left + 1);
    // update the last index of this character
    SEEN_LAST_AT[s[right]] = right;
  }
  return ans;
}