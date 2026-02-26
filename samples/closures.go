package main

func apply(f func(int) int, x int) int {
	return f(x)
}

func makeAdder(n int) func(int) int {
	return func(x int) int {
		return x + n
	}
}

func main() {
	// Non-capturing func literal passed as argument
	double := func(x int) int {
		return x * 2
	}
	println(apply(double, 5))

	// Func literal called immediately
	result := func(a int, b int) int {
		return a + b
	}(3, 4)
	println(result)

	// Higher-order function returning a func literal (captures n)
	add5 := makeAdder(5)
	println(add5(10))
}
