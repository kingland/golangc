package main

type Stringer interface {
	String() string
}

type MyInt int

func (m MyInt) String() string {
	return "MyInt"
}

func Print(s Stringer) {
	println(s.String())
}

func main() {
	var x MyInt = 42
	Print(x)
}
