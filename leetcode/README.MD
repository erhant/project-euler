## LeetCode

Use the LeetCode online editor to run these with respect to the problems.

### Index

To make an index out of these problems, use `make`. Each file will have a multi-line comment block followed by a blank line at the top. Within that comment, it will have a YAML text. For example:

```C
/*
topics:
  - string
  - sliding window
difficulty: easy
*/

```

The lines `/*`, `*/`, and the blank line ` ` are important. When you `make`, an `index.yaml` is created with entries such as:

```YAML
valid-parentheses.cpp:
  topics:
    - stack
    - string
  difficulty: easy
```
