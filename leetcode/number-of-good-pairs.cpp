// Use Histogram
class Solution {
 public:
  int numIdenticalPairs(vector<int>& nums) {
    int pairs = 0;
    int H[101] = {0};
    for (int& n : nums) {
      pairs += H[n]++;
    }
    return pairs;
  }
};

// Use HashMap
class Solution2 {
 public:
  int numIdenticalPairs(vector<int>& nums) {
    int pairs = 0;
    unordered_map<int, int> hash;
    for (int i = 0; i != nums.size(); i++) {
      if (hash.find(nums[i]) != hash.end()) {
        pairs += hash[nums[i]];
        hash[nums[i]]++;
      } else {
        hash[nums[i]] = 1;
      }
    }
    return pairs;
  }
};
