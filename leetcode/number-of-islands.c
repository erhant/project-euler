/*
topics:
  - array
  - dfs
  - bfs
  - union find
  - matrix
difficulty: medium
*/

void removeLand(char** grid, int rows, int cols, int r, int c) {
  if (r >= rows || c >= cols || grid[r][c] == '0') return;
  grid[r][c] = '0';
  if (r > 0 && grid[r - 1][c] == '1') removeLand(grid, rows, cols, r - 1, c);
  if (c > 0 && grid[r][c - 1] == '1') removeLand(grid, rows, cols, r, c - 1);
  if (r < rows - 1 && grid[r + 1][c] == '1') removeLand(grid, rows, cols, r + 1, c);
  if (c < cols - 1 && grid[r][c + 1] == '1') removeLand(grid, rows, cols, r, c + 1);
}

int numIslands(char** grid, int gridSize, int* gridColSize) {
  // idea: once you see a land, count that as 1 and recursively remove all connected land.
  // continue when you are done, and repeat.
  // when the matrix is finished, return the count.
  int rows = gridSize;
  int cols = gridColSize[0];
  int lands = 0;
  for (int r = 0; r < rows; r++) {
    for (int c = 0; c < cols; c++) {
      if (grid[r][c] == '1') {
        lands++;
        removeLand(grid, rows, cols, r, c);
      }
    }
  }
  return lands;
}

// A BFS solution would work better than recursive