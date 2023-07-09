package main

import "fmt"

// Number of days in months
var M = [12]int{31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31}

func countSundays() int {
	d := 2 // 1 Jan 1901 is a Tuesday
	ans := 0

	for y := 1901; y < 2001; y++ {
		// for years 1901, 1902, ..., 2000
		for m := 0; m < 12; m++ {
			// for every day in a month
			d += M[m]
			if (m == 1) && ((y % 4) == 0) {
				d++
			}
			if (d % 7) == 0 {
				ans++
			}
		}
	}

	return ans
}

func main() {
	fmt.Printf("Answer:\n%d\n", countSundays())
}

// A better method is possible with Gauss' method, but i really dont see any entertainment in this question.
