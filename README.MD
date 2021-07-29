My submissions for several programming problems.

# Makefile
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

Each directory is also capable of creating their own `index.yaml`. The outer `index.yaml` is an aggregation of these. All `index.yaml` files are linted using `yamllint`.