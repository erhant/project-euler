#define EXISTS(hash, key) (hash.find(key) != hash.end())

class Solution {
 public:
  bool checkIfExist(vector<int>& arr) {
    unordered_map<double, bool> hashA;
    unordered_map<double, bool> hashB;
    for (int& n : arr)
      if (EXISTS(hashA, n) || EXISTS(hashB, n))
        return true;
      else {
        hashA[double(n) * 2.0] = true;
        hashB[double(n) / 2.0] = true;
      }
    return false;
  }
};

// NOTE: can be improved