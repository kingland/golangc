package main

func sum(nums ...int) int {
	total := 0
	for _, n := range nums {
		total = total + n
	}
	return total
}

func max(first int, rest ...int) int {
	m := first
	for _, n := range rest {
		if n > m {
			m = n
		}
	}
	return m
}

func main() {
	println(sum(1, 2, 3))        // 6
	println(sum(10, 20, 30, 40)) // 100
	println(sum())               // 0

	nums := []int{4, 5, 6}
	println(sum(nums...)) // 15

	println(max(3, 1, 4, 1, 5, 9, 2, 6)) // 9
}
