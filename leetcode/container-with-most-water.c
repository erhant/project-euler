/*
topics:
  - array
  - two pointers
  - greedy
difficulty: medium
*/

#define MIN(a, b) ((a) < (b)) ? (a) : (b)
#define MAX(a, b) ((a) > (b)) ? (a) : (b)
#define AREA(i, j) (((j) - (i)) * (MIN(arr[i], arr[j])))

int maxArea(int* arr, int n){
  int area = 0;
  for (int i = 0, j = n - 1; i < j; arr[i] > arr[j] ? j-- : i++) 
    area = MAX(area, AREA(i, j));
  return area;
}