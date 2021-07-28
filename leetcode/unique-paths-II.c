/*
topics:
  - array
  - dynamic programming
  - matrix
difficulty: medium
*/

// 82.86% time, 100% mem, need to improve time.
int uniquePathsWithObstacles(int** obstacleGrid, int obstacleGridSize, int* obstacleGridColSize) {
  int m = obstacleGridSize;
  int n = obstacleGridColSize[0];
  if (obstacleGrid[0][0] == 1) return 0;
  obstacleGrid[0][0] = 1;
  // init borders
  for (int r = 1; r < m; r++) obstacleGrid[r][0] = ((obstacleGrid[r][0] == 0) ? obstacleGrid[r - 1][0] : 0);
  for (int c = 1; c < n; c++) obstacleGrid[0][c] = ((obstacleGrid[0][c] == 0) ? obstacleGrid[0][c - 1] : 0);
  // dp
  for (int r = 1; r < m; r++) {
    for (int c = 1; c < n; c++) {
      // printf("PATH[%d,%d] %d, PATH[%d,%d] %d\n",r-1,c,obstacleGrid[r-1][c],r,c-1,obstacleGrid[r][c-1]);
      obstacleGrid[r][c] = (obstacleGrid[r][c] == 0) ? (obstacleGrid[r][c - 1] + obstacleGrid[r - 1][c]) : 0;
    }
  }

  return obstacleGrid[m - 1][n - 1];
}