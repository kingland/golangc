package main

import (
	"fmt"
	"math"
	"strings"
)

func main() {
	// strings package demo
	s := "Hello, World!"
	fmt.Println(strings.ToUpper(s))
	fmt.Println(strings.ToLower(s))
	fmt.Println(strings.Contains(s, "World"))
	fmt.Println(strings.HasPrefix(s, "Hello"))
	fmt.Println(strings.HasSuffix(s, "World!"))
	fmt.Println(strings.Index(s, "World"))
	fmt.Println(strings.TrimSpace("  trimmed  "))
	fmt.Println(strings.Replace(s, "World", "Go", -1))
	fmt.Println(strings.Repeat("ab", 3))

	// for-range over string (UTF-8 rune iteration)
	word := "café"
	count := 0
	for range word {
		count++
	}
	fmt.Println(count) // 4 runes

	// math package demo
	fmt.Println(math.Sqrt(144.0))   // 12
	fmt.Println(math.Abs(-3.14))    // 3.14
	fmt.Println(math.Floor(3.9))    // 3
	fmt.Println(math.Ceil(3.1))     // 4
	fmt.Println(math.Round(3.5))    // 4
	fmt.Println(math.Max(10.0, 20.0)) // 20
	fmt.Println(math.Min(10.0, 20.0)) // 10
	fmt.Println(math.Pow(2.0, 8.0))   // 256
}
