package main

import (
	"fmt"
	"os"
)

func greet(name string) {
	fmt.Fprintf(os.Stdout, "Hello, %s!\n", name)
}

func warn(msg string) {
	fmt.Fprintf(os.Stderr, "Warning: %s\n", msg)
}

func main() {
	greet("world")
	warn("something went wrong")
	fmt.Fprintln(os.Stdout, "Done.")
	fmt.Fprintf(os.Stdout, "Exit code: %d\n", 0)
}
