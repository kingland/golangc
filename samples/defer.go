package main

func cleanup(msg string) {
	println(msg)
}

func doWork() {
	defer cleanup("deferred 1")
	defer cleanup("deferred 2")
	println("working")
}

func main() {
	doWork()
	println("done")
}
