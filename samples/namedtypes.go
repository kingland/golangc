package main

type Counter struct{ n int }

func (c *Counter) Inc() { c.n++ }
func (c *Counter) Value() int { return c.n }

type Direction int

const (
	North Direction = iota
	East
	South
	West
)

func (d Direction) Name() string {
	switch d {
	case North:
		return "North"
	case East:
		return "East"
	case South:
		return "South"
	default:
		return "West"
	}
}

func main() {
	c := Counter{}
	c.Inc()
	c.Inc()
	c.Inc()
	println(c.Value())

	println(South.Name())
}
