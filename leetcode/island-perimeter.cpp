/*
topics:
  - array
  - bfs
  - dfs
  - matrix
difficulty: easy
*/

class Solution {
 public:
  void processIsland(vector<vector<int>>& grid, int r, int c, int* ans) {
    grid[r][c] = -1;
    // Count perimeter
    if (r == 0 || (r > 0 && grid[r - 1][c] == 0)) (*ans)++;
    if (c == 0 || (c > 0 && grid[r][c - 1] == 0)) (*ans)++;
    if (r == grid.size() - 1 || (r < grid.size() - 1 && grid[r + 1][c] == 0)) (*ans)++;
    if (c == grid[0].size() - 1 || (c < grid[0].size() - 1 && grid[r][c + 1] == 0)) (*ans)++;
    // Proceed to other blocks
    if (r > 0 && grid[r - 1][c] == 1) processIsland(grid, r - 1, c, ans);
    if (c > 0 && grid[r][c - 1] == 1) processIsland(grid, r, c - 1, ans);
    if (r < grid.size() - 1 && grid[r + 1][c] == 1) processIsland(grid, r + 1, c, ans);
    if (c < grid[0].size() - 1 && grid[r][c + 1] == 1) processIsland(grid, r, c + 1, ans);
  }

  int islandPerimeter(vector<vector<int>>& grid) {
    for (int i = 0; i < grid.size(); i++) {
      for (int j = 0; j < grid[0].size(); j++) {
        if (grid[i][j] == 1) {
          int ans;
          processIsland(grid, i, j, &ans);
          return ans;
        }
      }
    }
    return 0;  // no island case, though not possible
  }
};