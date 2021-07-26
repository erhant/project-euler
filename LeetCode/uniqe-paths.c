#define A(r, c) (r) * (n) + (c)  // assumes n is defined

int uniquePaths(int m, int n) {
  int* paths = (int*)malloc(m * n * sizeof(int));  // matrix, but allocated as 1D
  // init borders
  for (int r = 0; r < m; r++) paths[A(r, 0)] = 1;
  for (int c = 1; c < n; c++) paths[A(0, c)] = 1;
  // dp
  for (int r = 1; r < m; r++) {
    for (int c = 1; c < n; c++) {
      // printf("PATH[r,c-1] %d, PATH[r-1,c] %d\n",paths[A(r,c-1)],paths[A(r-1,c)]);
      paths[A(r, c)] = paths[A(r - 1, c)] + paths[A(r, c - 1)];
    }
  }

  return paths[A(m - 1, n - 1)];
}

// Pascal Triangle Option
/*                                       line = m + n - 1
//                                   1 / row = n
//line >  1                        5 1
//       1  1                   10 4 1
//      1  2  1     --->      10 6 3 1 (m)
//     1  3  3  1            5 4 3 2 1
//    1  4  6  4  1        1 1 1 1 1 1
//   1  5  10 10 5  1           (n)
//      ^ row
//
//C(line, elem)   = line! / ( (line-elem)! * elem! )  = (line * (line - 1) * ... * (line - elem + 1)) / (elem!)
//line = m + n - 1
//elem = n
// however, this would have high chances of overflow, due to factorial
*/

// Recursive Solution | works, but exceeds time limit.
/*
int uniquePaths(int m, int n){
  return (m == 1 && n == 1) ? 1 : (((m == 1) ? 0 : uniquePaths(m-1, n)) + ((n == 1) ? 0 : uniquePaths(m, n-1)));
}

*/