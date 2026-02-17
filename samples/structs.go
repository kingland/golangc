package main

type Point struct {
	X, Y int
}

func (p Point) Add(other Point) Point {
	return Point{p.X + other.X, p.Y + other.Y}
}

func main() {
	p1 := Point{1, 2}
	p2 := Point{3, 4}
	p3 := p1.Add(p2)
	println(p3.X, p3.Y)
}
