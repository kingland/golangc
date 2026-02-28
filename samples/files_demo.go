package main

import (
	"fmt"
	"os"
	"strconv"
)

func main() {
	f, _ := os.Create("output.txt")
	f.WriteString("hello, file!\n")
	f.Close()

	n, _ := strconv.ParseInt("123", 10, 64)
	x, _ := strconv.ParseFloat("3.14", 64)
	fmt.Fprintf(os.Stdout, "n=%d x=%s\n", n, strconv.FormatFloat(x, 'f', 2, 64))

	fmt.Println(strconv.FormatBool(true))
	os.Exit(0)
}
