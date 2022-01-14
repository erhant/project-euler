package main

import "fmt"

// Initialize the lattice paths grid
func initGrid(rows int, cols int) [][]int {
	// create
	grid := make([][]int, rows)
	for i := 0; i < rows; i++ {
		grid[i] = make([]int, cols)
	}
	// initialize
	for i := 0; i < rows; i++ {
		grid[i][0] = 1
	}
	for i := 0; i < cols; i++ {
		grid[0][i] = 1
	}
	return grid
}

// Calculate number of paths from top left to bottom right
func latticePaths(rows int, cols int) int {
	// increase row and col by 1, because you care about lines, not faces of the grid
	rows += 1
	cols += 1
	var grid [][]int = initGrid(rows, cols)
	for r := 1; r < rows; r++ {
		for c := 1; c < cols; c++ {
			grid[r][c] = grid[r-1][c] + grid[r][c-1]
		}
	}
	return grid[rows-1][cols-1]
}

func main() {
	rows := 20
	cols := 20
	fmt.Printf("Answer:\nA %d x %d grid has %d paths.\n", rows, cols, latticePaths(rows, cols))
}
