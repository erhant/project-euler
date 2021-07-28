/*
topics:
  - math
  - dynamic programming
  - memoization
difficulty: easy
*/
int climbStairs(int n) {
  if (n == 1) return 1;
  if (n == 2) return 2;
  // 2 variable Fibonacci
  int a = 1;
  int b = 2;
  for (int i = 2; i < n; i++) {
    b = a + b;
    a = b - a;  // a + b_prev - a = b_prev
  }
  return b;
}

// Recursive (Exceeds Time)
/*
int climbStairs(int n){
  if (n == 1) return 1;
  if (n == 2) return 2;
  return climbStairs(n-1) + climbStairs(n-2);
}
*/

// Memory Inefficient DP Way
/*
int climbStairs(int n){
  if (n == 1) return 1;
  if (n == 2) return 2;
  int* history = (int *)malloc(n * sizeof(int));
  history[0] = 1;
  history[1] = 2;
  for (int i = 2; i<n; i++) {
    history[i] = history[i-1] + history[i-2];
  }
  return history[n-1]; // n is here (1 indexed to 0 indexed)
  // basic idea: to reach N you can do: [1 step + (N-1)] or [2 step + (N-2)], so you basically recurse
}
*/