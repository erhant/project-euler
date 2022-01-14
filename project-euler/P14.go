package main

import "fmt"

var MEM = make(map[int]int)

func Collatz_length(n int) int {
	ans := 1
	for n > 1 {
		if memlen, ok := MEM[n]; ok {
			return ans + memlen
		} else {
			ans++
			if n&1 == 0 {
				n >>= 1
			} else {
				n = 3*n + 1
			}
		}
	}
	return ans
}

func main() {
	lim := 1000000
	l := 0
	maxl := 0
	maxn := 0
	for n := 1; n < lim; n++ {
		l = Collatz_length(n)
		if l > maxl {
			maxl = l
			maxn = n
		}
	}

	fmt.Printf("Answer:\n%d has max length %d\n", maxn, maxl)
}
