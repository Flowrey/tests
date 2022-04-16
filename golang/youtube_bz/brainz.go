package main

import (
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
)

type Release struct {
	Title        string         `json:"title"`
	ArtistCredit []ArtistCredit `json:"artist-credit"`
	Media        []Media        `json:"media"`
}

type ArtistCredit struct {
	Name string `json:"name"`
}

type Media struct {
	Tracks []Track `json:"tracks"`
}

type Track struct {
	Title  string `json:"title"`
	Length int    `json:"length"`
}

func brainz_search(mbid string) Release {
	var url string = fmt.Sprintf("https://musicbrainz.org/ws/2/release/%s?inc=artists+recordings&fmt=json", mbid)
	resp, err := http.Get(url)
	if err != nil {
		fmt.Fprintf(os.Stderr, "error: %v\n", err)
		os.Exit(1)
	}

	defer resp.Body.Close()
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		fmt.Fprintf(os.Stderr, "error: %v\n", err)
		os.Exit(1)
	}

	jsonBlob := string(body)
	if err != nil {
		fmt.Fprintf(os.Stderr, "error: %v\n", err)
		os.Exit(1)
	}

	var release Release
	json.Unmarshal([]byte(jsonBlob), &release)

	return release
}
