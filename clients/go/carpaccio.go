package main

type Order struct {
	Prices     []float32
	Quantities []int
	Country    string
	Reduction  string
}

type Reply struct {
	Total float32 `json:"total"`
}
