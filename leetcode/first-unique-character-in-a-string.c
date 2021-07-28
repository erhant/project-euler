/*
topics:
  - string
  - hash table
  - queue
  - counting
difficulty: easy
*/

int firstUniqChar(char* s) {
  int COUNT[26] = {0}, i;
  for (i = 0; s[i] && ++COUNT[s[i] - 'a']; i++)
    ;
  for (i = 0; s[i] && COUNT[s[i] - 'a'] != 1; i++)
    ;
  return s[i] == '\0' ? -1 : i;
}

/*
Deliberately ugly :)
*/