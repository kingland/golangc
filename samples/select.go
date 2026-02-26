package main

func producer(ch chan int, val int) {
	ch <- val
}

func main() {
	ch1 := make(chan int)
	ch2 := make(chan int)

	go producer(ch1, 10)

	// ch1 will have a value ready; ch2 will not
	select {
	case v := <-ch1:
		println(v)
	case v := <-ch2:
		println(v)
	}

	// Default fires when no channel is ready
	ch3 := make(chan int)
	select {
	case v := <-ch3:
		println(v)
	default:
		println(99)
	}

	// Direct receive (non-select)
	ch4 := make(chan int)
	go producer(ch4, 42)
	v := <-ch4
	println(v)
}
