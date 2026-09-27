package main

import (
	"io"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
)

func TestPostPingReturnsPong(t *testing.T) {
	mux := setupMux()
	server := httptest.NewServer(mux)
	defer server.Close()

	resp, err := http.Post(server.URL+"/ping", "text/plain", strings.NewReader(""))
	if err != nil {
		t.Fatalf("Failed to make request: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		t.Errorf("Expected status 200, got %d", resp.StatusCode)
	}

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		t.Fatalf("Failed to read response body: %v", err)
	}

	if string(body) != "pong" {
		t.Errorf("Expected body 'pong', got '%s'", string(body))
	}
}
