package main

import (
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"regexp"
)

type YoutubeInitialData struct {
	Contents struct {
		TwoColumnSearchResultsRenderer TwoColumnSearchResultsRenderer
	}
}

type TwoColumnSearchResultsRenderer struct {
	PrimaryContents PrimaryContents
}

type PrimaryContents struct {
	SectionListRenderer SectionListRenderer
}

type SectionListRenderer struct {
	Contents []struct {
		ItemSectionRenderer ItemSectionRenderer
	}
}

type ItemSectionRenderer struct {
	Contents []struct {
		VideoRenderer VideoRenderer `json:"videoRenderer,omitempty"`
	}
}

type VideoRenderer struct {
	VideoID string `json:"videoId"`
	Title   struct {
		Runs []struct {
			Text string `json:"text"`
		} `json:"runs"`
	} `json:"title"`
	LongBylineText struct {
		Runs []struct {
			Text string `json:"text"`
		} `json:"runs"`
	} `json:"longBylineText"`
	LengthText struct {
		SimpleText string `json:"simpleText"`
	} `json:"lengthText"`
	ViewCountText struct {
		SimpleText string `json:"simpleText"`
	} `json:"viewCountText"`
	OwnerBadges []struct {
		MetadataBadgeRenderer struct {
			Icon struct {
				IconType string `json:"iconType"`
			} `json:"icon"`
			Style             string `json:"style"`
			Tooltip           string `json:"tooltip"`
			TrackingParams    string `json:"trackingParams"`
			AccessibilityData struct {
				Label string `json:"label"`
			} `json:"accessibilityData"`
		} `json:"metadataBadgeRenderer"`
	} `json:"ownerBadges"`
}

type Videos struct {
	title   []string
	channel []string
	id      []string
	length  []string
	view    []string
}

func youtube_search(search_query string) Videos {
	var url string = fmt.Sprintf("https://youtube.com/results?search_query=%s", search_query)
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

	r, err := regexp.Compile("var ytInitialData = (.*);</script><script")
	jsonBlob := r.FindStringSubmatch(string(body))[1]
	if err != nil {
		fmt.Fprintf(os.Stderr, "error: %v\n", err)
		os.Exit(1)
	}

	var ytInitialData YoutubeInitialData
	var videos Videos
	json.Unmarshal([]byte(jsonBlob), &ytInitialData)
	for _, content := range ytInitialData.Contents.TwoColumnSearchResultsRenderer.PrimaryContents.SectionListRenderer.Contents[0].ItemSectionRenderer.Contents {
		if len(content.VideoRenderer.OwnerBadges) > 0 {
			if len(content.VideoRenderer.Title.Runs) > 0 {
				videos.title = append(videos.title, content.VideoRenderer.Title.Runs[0].Text)
				videos.channel = append(videos.channel, content.VideoRenderer.LongBylineText.Runs[0].Text)
				videos.id = append(videos.id, content.VideoRenderer.VideoID)
				videos.view = append(videos.view, content.VideoRenderer.ViewCountText.SimpleText)
				videos.length = append(videos.length, content.VideoRenderer.LengthText.SimpleText)
			}
		}
	}
	return videos
}
