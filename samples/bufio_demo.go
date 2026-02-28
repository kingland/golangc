package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	// Read file line by line with Scanner
	f, _ := os.Open("input.txt")
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		fmt.Println(scanner.Text())
	}
	f.Close()

	// Read from stdin with bufio.Reader
	reader := bufio.NewReader(os.Stdin)
	line, _ := reader.ReadString('\n')
	fmt.Println(line)

	// Read whole file
	data, _ := os.ReadFile("input.txt")
	_ = data
}
