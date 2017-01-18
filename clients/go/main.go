package main

import (
	"encoding/json"
	"io/ioutil"
	"log"
	"net/http"
	"os"

	"fmt"
)

type Order struct {
	Prices     []float32
	Quantities []int
	Country    string
	Reduction  string
}

type Reply struct {
	Total float32 `json:"total"`
}

func main() {
	http.HandleFunc("/order", handler)
	http.HandleFunc("/feedback", func(rw http.ResponseWriter, req *http.Request) {
		defer req.Body.Close()

		body, err := ioutil.ReadAll(req.Body)
		if err != nil {
			fmt.Printf("error reading body: %v\n", err)
			rw.WriteHeader(204)
			return
		}

		fmt.Printf("Feedback: %s\n", body)

		rw.WriteHeader(200)
	})

	err := http.ListenAndServe(fmt.Sprintf(":%s", getPort()), nil)
	if err != nil {
		log.Fatal("Listen and serve:", err)
	}
}

func handler(rw http.ResponseWriter, req *http.Request) {
	defer req.Body.Close()

	body, err := ioutil.ReadAll(req.Body)
	if err != nil {
		fmt.Printf("error reading body: %v\n", err)
		rw.WriteHeader(204)
		return
	}

	var order Order
	json.Unmarshal(body, &order)

	fmt.Printf("Got order: %#v\n", order)

	rw.Header().Add("Content-Type", "application/json")
	rw.WriteHeader(200)
	//	encoder := json.NewEncoder(rw)
	//	encoder.Encode(Reply{0})
}

func getPort() string {
	port := os.Getenv("PORT")
	if port == "" {
		return "9000"
	}
	return port
}
