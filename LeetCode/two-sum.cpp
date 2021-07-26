class Solution {
 public:
  vector<int> twoSum(vector<int>& nums, int target) {
    unordered_map<int, int> hash;  // HashMap that maps target-num -> numIndex for nums array
    for (int i = 0; i < nums.size(); i++) {
      if (hash.find(nums[i]) != hash.end()) {
        return vector<int>{i, hash[nums[i]]};
      }
      hash[target - nums[i]] = i;
    }
    return vector<int>{-1, -1};  // no results
  }
};