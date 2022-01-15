package main

import "fmt"

// Number of days in months
var DIM = [...]int{31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31}
var DIM_LEAP = [...]int{31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31}

func countSundays(fromyear, toyear int) int {
	numSundays := 0

	for y := fromyear; y < toyear; y++ {
		for m := 0; m < 12; m++ {

		}
	}
}

func main() {
	lim := 1000
	sum := 0
	for n := 1; n <= lim; n++ {
		sum += 3
	}
	fmt.Printf("Answer:\n%d\n", sum)
}

// A better method is possible with Gauss' method, but i really dont see any entertainment in this question.
