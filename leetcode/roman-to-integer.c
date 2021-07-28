/*
topics:
  - math
  - string
  - hashmap
difficulty: easy
*/

int romanToInt(char* s) {
  unsigned short ans = 0;
  char prev = '\0';
  for (char i = 0; s[i] != '\0'; i++) {
    switch (s[i]) {
      case 'I':
        ans += 1;
        break;
      case 'V':  // I+V = 6, IV = 4, I = 1 --> IV should add 3
        ans += (prev == 'I') ? 3 : 5;
        break;
      case 'X':  // I+X = 11, IX = 9, I = 1 --> IX should add 8
        ans += (prev == 'I') ? 8 : 10;
        break;
      case 'L':  // X+L = 60, LX = 40, X = 10 --> LX should add 30
        ans += (prev == 'X') ? 30 : 50;
        break;
      case 'C':  // X+C = 110, LX = 90, X = 10 --> LX should add 80
        ans += (prev == 'X') ? 80 : 100;
        break;
      case 'D':  // C+D = 600, CD = 400, C = 100 --> CD should add 300
        ans += (prev == 'C') ? 300 : 500;
        break;
      case 'M':  // C+M = 1100, CM = 900, C = 100 --> CM should add 800
        ans += (prev == 'C') ? 800 : 1000;
        break;
      default:
        continue;
    }
    printf("%hu\t%c\t%c\n", ans, prev, s[i]);
    prev = s[i];
  }

  return ans;
}