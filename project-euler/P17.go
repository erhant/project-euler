package main

import "fmt"

var ONES = [20]int{
	4, // zero
	3, // one
	3, // two
	5, // three
	4, // four
	4, // five
	3, // six
	5, // seven
	5, // eight
	4, // nine
	3, // ten
	6, // eleven
	6, // twelve
	8, // thirteen
	8, // fourteen
	7, // fifteen
	7, // sixteen
	9, // seventeen
	8, // eighteen
	8, // nineteen
}

var TENS = [10]int{
	4, // zero
	3, // ten
	6, // twenty
	6, // thirty
	5, // forty
	5, // fifty
	5, // sixty
	7, // seventy
	6, // eighty
	6, // ninety
}

func num_letters(n int) int {
	if n < 0 {
		panic("input is less than 0")
	} else if n < 20 {
		// 1..19 is given in ONES
		return ONES[n]
	} else if (n <= 90) && (n%10) == 0 {
		// any of the tenth is given in TENS
		return TENS[int(n/10)]
	} else if n < 100 {
		// TENS + ONES is given as such
		return TENS[int(n/10)] + ONES[n%10]
	} else if (n < 1000) && (n%100 == 0) {
		// any of the hundreds is given in ONES + hundred
		return ONES[int(n/100)] + 7 // +7 for "hundred"
	} else if n < 1000 {
		// other hundreds are ONES + (num for tenth)
		return ONES[int(n/100)] + 10 + num_letters(n%100) // +10 for "hundred and"
	} else if n == 1000 {
		// one thousand is special
		return 11 // +11 for "one thousand"
	} else {
		panic("input is more than 1000")
	}

}

func main() {
	lim := 1000
	sum := 0
	for n := 1; n <= lim; n++ {
		sum += num_letters(n)
	}
	fmt.Printf("Answer:\n%d\n", sum)
}

// NOTE: https://stemhash.com/counting-lattice-paths/
// A better method is possible by using combinatorics.
