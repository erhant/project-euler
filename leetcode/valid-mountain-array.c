bool validMountainArray(int* arr, int n) {
  if (n < 3) return false;
  int i = 1;
  for (; i < n && arr[i] > arr[i - 1]; i++)
    ;
  for (; i < n && arr[i] < arr[i - 1]; i++)
    ;
  return i == n && arr[1] > arr[0] && arr[n - 1] < arr[n - 2];
}

// See my post: https://leetcode.com/problems/valid-mountain-array/discuss/1365940/C-or-Simple-O(n)-solution-5-lines