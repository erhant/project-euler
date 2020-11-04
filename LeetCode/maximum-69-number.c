// Change the first 6 to a 9
int maximum69Number (int num){
  int div = 1000;
  while (div > 0 && ((num % (10 * div)) - (num % div)) / div != 6) div /= 10;
  return (div > 0) ? (num + 3*div) : num;
}

/*
The operation below gets you the digit: (though it is a bit expensive)
  
  (N mod (10 * D)) - (N mod D) 
  ---------------------------- = digit at D'th place (XYZ --> X for D=100, Y for D=10, Z for D=1)
                D
 
 We basically do this, and then check if the digit is 6.
 When that is the case, just do N = N - 6*D + 9*D = N + 3*D

*/