void rotate(int** matrix, int matrixSize, int* matrixColSize){
  int tmp;
  int n = matrixSize - 1;
  for (int i = 0; i < matrixSize / 2; i++) {
    for (int j = i; j < n - 1 - i; j++) {
      tmp = matrix[i][j];
      matrix[i][j] = matrix[n-i-j][i];
      matrix[n-i-j][i] = matrix[n-i][n-i-j];
      matrix[n-i][n-i-j] = matrix[j][n-i];
      matrix[j][n-i] = tmp;
    }
  }
  // todo: not working
}