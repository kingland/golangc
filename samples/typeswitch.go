package main

func typeOf(i interface{}) string {
	switch i.(type) {
	case int:
		return "int"
	case string:
		return "string"
	case bool:
		return "bool"
	default:
		return "other"
	}
}

func main() {
	println(typeOf(42))
	println(typeOf("hello"))
	println(typeOf(true))
}
