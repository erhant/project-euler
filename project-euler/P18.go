package main

// proposed solution: make a tree out of these, and then start from the bottom
// in each node: value += max(leftValue ,rightValue)

func main() {
	print(TREE[SIZE-1])
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
