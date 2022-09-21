To check if a key exists in an `unordered_map`:

```Cpp
#define EXISTS(hash, key) (hash.find(key) != hash.end())
```

---

When you need to alternate a variable, say `p` and `q`, you can do the following:

```C
int altvar[2] = {p, q};
bool a = 0;
int x = altvar[a]; // to access
a = !a; // to switch variable
```

---

To get the number of digits from an `int`:

```C
int numDigits(int n) {
  if (n < 100000)
    if (n < 100)
      if (n < 10)
        return 1;
      else
        return 2;
    else
      if (n < 1000)
        return 3;
      else
        if (n < 10000)
          return 4;
        else
          return 5;
  else
    if (n < 10000000)
      if (n < 1000000)
        return 6;
      else
        return 7;
    else
      if (n < 100000000)
        return 8;
      else
        if (n < 1000000000)
          return 9;
        else
          return 10; // cant be more
}
```

<https://go.dev/doc/code>
