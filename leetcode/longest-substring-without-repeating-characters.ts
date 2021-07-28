/*
topics:
  - string
  - hash table
  - sliding window
difficulty: medium
*/

function lengthOfLongestSubstring(s: string): number {
  let seen: { [id: string]: number; } = {};
  let ans: number = 0, left: number = 0, right: number = 0;
  for (; right < s.length; right++) {
    if (s[right] in seen)
      left = Math.max(left, seen[s[right]]! + 1);
    seen[s[right]] = right;
    ans = Math.max(ans, right - left + 1);
  }
  return ans;
};

// uses linear time