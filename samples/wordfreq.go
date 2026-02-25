package main

func main() {
	// Test map len
	m := make(map[string]int)
	m["hello"] = 3
	m["world"] = 1
	m["hello"] = 5
	println(len(m))

	// Test delete
	delete(m, "world")
	println(len(m))

	// Test for-range map (just count iterations)
	count := 0
	for k, v := range m {
		_ = k
		_ = v
		count = count + 1
	}
	println(count)

	// Test slice append
	s := make([]int, 0)
	s = append(s, 10)
	s = append(s, 20)
	s = append(s, 30)
	println(len(s))
	println(s[0])
	println(s[1])
	println(s[2])
}
