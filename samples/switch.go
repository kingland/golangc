package main

func classify(n int) string {
	switch n {
	case 0:
		return "zero"
	case 1, 2, 3:
		return "small"
	case 10:
		return "ten"
	default:
		return "other"
	}
}

func grade(score int) string {
	switch {
	case score >= 90:
		return "A"
	case score >= 80:
		return "B"
	case score >= 70:
		return "C"
	default:
		return "F"
	}
}

func main() {
	println(classify(0))
	println(classify(2))
	println(classify(10))
	println(classify(99))
	println(grade(95))
	println(grade(85))
	println(grade(65))
}
