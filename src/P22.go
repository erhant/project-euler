package main

import (
	"os"
	"sort"
	"strings"
)

func readNames(path string) []string {
	input, err := os.ReadFile(path)
	if err != nil {
		panic(err)
	}
	str := string(input[:])
	arr := strings.Split(str, "\",\"")
	arr[0] = strings.TrimPrefix(arr[0], "\"")                   // remove the first char (") from first string
	arr[len(arr)-1] = strings.TrimSuffix(arr[len(arr)-1], "\"") // remove the last char (") from last string
	sort.Strings(arr)                                           // now sort them
	return arr
}

func nameToScore(index uint64, name string) uint64 {
	var nameScore uint64 = 0
	for _, c := range name {
		nameScore += uint64(c - 'Z' + 26)
	}
	return nameScore * (index + 1)
}

func sumNameScores(names []string) uint64 {
	var score uint64 = 0
	var i uint64 = 0
	for ; i < uint64(len(names)); i++ {
		score += nameToScore(i, names[i])
	}
	return score
}

func main() {
	println("Answer:")
	println(sumNameScores(readNames("./res/p022_names.txt")))
}
