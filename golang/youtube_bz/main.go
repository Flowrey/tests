package main

import (
	"fmt"
	"io"
	"net/url"
	"os"
	"strings"

	"github.com/kkdai/youtube/v2"
	"github.com/schollz/closestmatch"
)

func main() {
	if len(os.Args) < 2 {
		fmt.Printf("help: %s [mbid]\n", os.Args[0])
		os.Exit(1)
	}

	var mbid string = os.Args[1]
	var bz_result Release = brainz_search(mbid)

	bagSizes := []int{2, 3, 4}
	for _, track := range bz_result.Media[0].Tracks {
		var query string = fmt.Sprintf("\"%s\" \"%s\" \"%s\" \"Auto-generated\"", bz_result.ArtistCredit[0].Name, bz_result.Title, track.Title)
		var video Videos = youtube_search(url.QueryEscape(query))

		cm := closestmatch.New(video.title, bagSizes)
		goodVideoTitle := cm.Closest(strings.ToLower(track.Title))
		for i := range video.title {
			if video.title[i] == goodVideoTitle {
				var videoID string = video.id[i]
				var fileNameRaw string = fmt.Sprintf("./test/%s.mp3", video.title[i])
				var fileName string = strings.Replace(fileNameRaw, "?", "", -1)

				client := youtube.Client{}

				video, err := client.GetVideo(videoID)
				if err != nil {
					panic(err)
				}

				formats := video.Formats.WithAudioChannels() // only get videos with audio
				stream, _, err := client.GetStream(video, &formats[0])
				if err != nil {
					panic(err)
				}

				file, err := os.Create(fileName)
				if err != nil {
					panic(err)
				}
				defer file.Close()

				_, err = io.Copy(file, stream)
				if err != nil {
					panic(err)
				}
			}
			continue
		}
	}
}
