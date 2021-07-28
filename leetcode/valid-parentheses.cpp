/*
topics:
  - stack
  - string
difficulty: easy
*/

class Solution {
 public:
  bool isValid(string s) {
    stack<char> stk;
    stk.push(s[0]);
    char c, top;
    for (int i = 1; i < s.length(); i++) {
      c = s[i];
      if (stk.size() > 0) {
        top = stk.top();
        // printf("c: %c\ttop: %c\n",c,top);
        if ((top == '(' && c == ')') || (top == '[' && c == ']') || (top == '{' && c == '}')) {
          stk.pop();
        } else if ((top != '(' && c == ')') || (top != '[' && c == ']') || (top != '{' && c == '}')) {
          return false;
        } else {
          stk.push(c);
        }
      } else {
        if (c == ')' || c == ']' || c == '}') {
          return false;
        } else {
          stk.push(c);
        }
      }
    }
    return (stk.size() == 0);
  }
};
// O(n) complexity, how to improve space?