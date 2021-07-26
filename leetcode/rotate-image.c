/*
right = matrixSize - 1 - i;
left = i;
up = i;
down = matrixSize - 1 - i;
*/
void rotate(int** matrix, int matrixSize, int* matrixColSize) {
  int tmp;
  for (int i = 0; i < matrixSize / 2; i++) {
    for (int j = 0; j < matrixSize - 1 - 2 * i; j++) {
      tmp = matrix[i][i + j];
      matrix[i][i + j] = matrix[matrixSize - 1 - i - j][i];
      matrix[matrixSize - 1 - i - j][i] = matrix[matrixSize - 1 - i][matrixSize - 1 - i - j];
      matrix[matrixSize - 1 - i][matrixSize - 1 - i - j] = matrix[i + j][matrixSize - 1 - i];
      matrix[i + j][matrixSize - 1 - i] = tmp;
    }
  }
}
// O(N^2) time, O(1) space