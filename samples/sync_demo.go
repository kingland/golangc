package main

import (
	"fmt"
	"sync"
)

var counter int
var mu sync.Mutex

func increment(wg *sync.WaitGroup) {
	mu.Lock()
	counter = counter + 1
	mu.Unlock()
	wg.Done()
}

func main() {
	var wg sync.WaitGroup

	// Launch 5 goroutines, each incrementing the shared counter
	wg.Add(5)
	go increment(&wg)
	go increment(&wg)
	go increment(&wg)
	go increment(&wg)
	go increment(&wg)

	// Wait for all goroutines to finish
	wg.Wait()

	fmt.Println(counter)

	// Demonstrate TryLock
	var m sync.Mutex
	if m.TryLock() {
		fmt.Println("acquired")
		m.Unlock()
	}
}
