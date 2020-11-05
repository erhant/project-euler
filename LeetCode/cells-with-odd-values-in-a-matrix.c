int oddCells(int n, int m, int** indices, int indicesSize, int* indicesColSize){
  int* rows = (int*)calloc(n, sizeof(int)); // calloc sets memory to 0.
  int* cols = (int*)calloc(m, sizeof(int)); // calloc sets memory to 0.
  for (int i = 0; i<indicesSize; i++) {
    rows[indices[i][0]]++;
    cols[indices[i][1]]++;
  }
  
  // could we improve this part?
  int count = 0;
  for(int i = 0; i < n; i++) 
    for(int j = 0; j < m; j++) 
        if((rows[i] + cols[j]) & 1) count++;
  return count;
}