class Solution {
public:
  vector<int> findDuplicates(vector<int>& nums) {
    // Paint negatives (as in first-missing-positive)   
    vector<int> ans;
    int num;
    for (int i = 0; i<nums.size(); i++) {
      num = (nums[i] < 0) ? -nums[i] : nums[i];
      if (nums[num-1] < 0) {
        ans.push_back(num); // they appear twice so I dont have to check if i added it before.
      } else {
        nums[num-1] = -nums[num-1]; // negate, as in Paint
      }     
    }
    return ans;
  }
};