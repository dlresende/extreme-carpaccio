package main

import (
	"fmt"
	"log"
	"net/http"
	"os"
)

func setupMux() *http.ServeMux {
	mux := http.NewServeMux()
	mux.HandleFunc("/ping", func(w http.ResponseWriter, r *http.Request) {
		if r.Method != http.MethodPost {
			http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
			return
		}
		w.Header().Set("Content-Type", "text/plain")
		w.WriteHeader(http.StatusOK)
		w.Write([]byte("pong"))
	})
	return mux
}

func getPort() string {
	port := os.Getenv("PORT")
	if port == "" {
		return "3000"
	}
	return port
}

func main() {
	port := getPort()
	mux := setupMux()
	log.Printf("Go client listening on port %s", port)
	if err := http.ListenAndServe(fmt.Sprintf(":%s", port), mux); err != nil {
		log.Fatalf("Server failed: %v", err)
	}
}
