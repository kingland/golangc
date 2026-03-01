package main

import (
	"fmt"
	"os"
	"sort"
	"strings"
)

func main() {
	// os.Getenv
	home := os.Getenv("HOME")
	fmt.Println("HOME=" + home)

	// strings.Fields / strings.TrimPrefix / strings.TrimSuffix
	line := "the quick brown fox"
	words := strings.Fields(line)
	sort.Strings(words)
	fmt.Println(strings.Join(words, ","))

	parts := strings.Split("a:b:c", ":")
	_ = parts

	fmt.Println(strings.TrimPrefix("foobar", "foo"))
	fmt.Println(strings.TrimSuffix("hello.go", ".go"))

	// sort.Ints
	nums := []int{5, 3, 1, 4, 2}
	sort.Ints(nums)
	for _, n := range nums {
		fmt.Println(n)
	}

	// fmt.Sscanf
	var x, y int
	fmt.Sscanf("10 20", "%d %d", &x, &y)
	fmt.Println(x + y)

	// fmt.Scan from stdin
	var z int
	fmt.Scan(&z)
	fmt.Println(z)
}
