package main

import "fmt"

var TREE = [...]int{75, 95, 64, 17, 47, 82, 18, 35, 87, 10, 20, 4, 82, 47, 65, 19, 1, 23, 75, 3, 34, 88, 2, 77, 73, 7, 63, 67, 99, 65, 4, 28, 6, 16, 70, 92, 41, 41, 26, 56, 83, 40, 80, 70, 33, 41, 48, 72, 33, 47, 32, 37, 16, 94, 29, 53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14, 70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57, 91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48, 63, 66, 4, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31, 4, 62, 98, 27, 23, 9, 70, 98, 73, 93, 38, 53, 60, 4, 23}

// Max of 2 integers
func MAX(n int, m int) int {
	if n > m {
		return n
	}
	return m
}

// Returns the number of elements in a given line
func LINE_IN(l int) int {
	return LINE_UPTO(l) - LINE_UPTO(l-1)
}

// Returns the number of elements up to a given line
func LINE_UPTO(l int) int {
	return int((l * (l + 1)) / 2)
}

// Finds the number of lines in a given tree (structure specific)
func FIND_LINECOUNT() int {
	var numelems int = len(TREE)
	var numlines int = 0
	var lim int = 0
	for numelems > 0 {
		numlines++
		lim++
		i := lim
		for i > 0 {
			i--
			numelems--
		}
	}
	return numlines
}

// Generate an array of indexes for values in the tree.
func MAKE_LEFT_INDEX(linecount int) []int {
	var L []int = make([]int, LINE_UPTO(linecount-1))
	for line := 1; line < linecount; line++ {
		k := LINE_IN(line)
		for i := LINE_UPTO(line - 1); i < LINE_UPTO(line-1)+k; i++ {
			L[i] = i + k
		}
	}
	return L
}

func findMaxPath() int {
	var linecount int = FIND_LINECOUNT()
	var L []int = MAKE_LEFT_INDEX(linecount) // L[i] -> index of the left child of node i
	var i int = LINE_UPTO(linecount-1) - 1   // rightmost element at the line just above the last
	for ; i >= 0; i-- {
		TREE[i] += MAX(TREE[L[i]], TREE[L[i]+1])
	}
	return TREE[0] // the root has max
}

// proposed solution: make a tree out of these, and then start from the bottom
// in each node: value += max(leftValue ,rightValue)
func main() {
	fmt.Printf("Answer:\n%d\n", findMaxPath())
}

/*
   3
  7 4
 2 4 6
8 5 9 3

   3    |      3      |      3      |      23     | -> 23 is the answer
  7 4   |    7   4    |    20 19    |    20 19    |
 2 4 6  |  10 13 15   |  10 13 15   |  10 13 15   |
8 5 9 3 | 8   5  9  3 | 8   5  9  3 | 8   5  9  3 |

Indexing

   1
  2 3
 4 5 6
7 8 9 10

*/
