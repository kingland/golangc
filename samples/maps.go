package main

func main() {
	m := make(map[string]int)
	m["hello"] = 42
	m["world"] = 99
	println(m["hello"])
	println(m["world"])
}
