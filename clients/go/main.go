package main

import (
	"encoding/json"
	"io/ioutil"
	"net/http"

	"fmt"
)

func main() {
	http.HandleFunc("/order", handler)
	http.HandleFunc("/feedback", func (rw http.ResponseWriter, req *http.Request) {
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
	http.ListenAndServe(":6666", nil)
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
