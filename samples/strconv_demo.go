package main

import (
	"fmt"
	"strconv"
	"os"
)

func main() {
	// strconv.Itoa: integer to string
	n := 42
	s := strconv.Itoa(n)
	fmt.Println("The answer is: " + s)

	// strconv.Atoi: string to integer
	x, err := strconv.Atoi("100")
	if err != nil {
		fmt.Println("parse error")
	} else {
		fmt.Println(x + 1) // prints 101
	}

	// fmt.Sprintf formatting
	msg := fmt.Sprintf("Value: %d", n)
	fmt.Println(msg)

	// string(rune) conversion — 'A' = 65
	ch := string(65)
	fmt.Println(ch)

	// os.Args
	fmt.Println(len(os.Args))
}
