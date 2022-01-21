/** Algorithm (from https://en.wikipedia.org/wiki/Permutation#Generation_in_lexicographic_order)
  1. Find the largest index k such that a[k] < a[k + 1].
     If no such index exists, the permutation is the last permutation.

  2. Find the largest index l greater than k such that a[k] < a[l].

  3. Swap the value of a[k] with that of a[l].

  4. Reverse the sequence from a[k + 1] up to and including the final element a[n].
  */
int next_permutation(int n, char** s) {
  int k, l, i, j;
  char* tmp;

  // 1. Find k
  for (i = n - 1; i > 0; i--) {
    if (strcmp(s[i - 1], s[i]) < 0) {
      k = i - 1;
      break;
    }
  }
  if (i == 0) return 0;

  // 2. Find l
  for (i = n - 1; i > k; i--) {
    if (strcmp(s[k], s[i]) < 0) {
      l = i;
      break;
    }
  }

  // 3. Swap k and l
  tmp = s[k];
  s[k] = s[l];
  s[l] = tmp;

  // 4. Reverse from k+1 and up
  i = k + 1;
  j = n - 1;
  while (i < j) {
    tmp = s[i];
    s[i] = s[j];
    s[j] = tmp;
    i++;
    j--;
  }

  return 1;
}