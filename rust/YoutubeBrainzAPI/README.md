# YoutubeBrainzAPI

REST API to YoutubeBrainz

## Usage

Run the server and ask for an mbid release: https://musicbrainz.org/doc/MusicBrainz_Identifier

```shell
$ cargo run --release
curl http://127.0.0.1:8080/release/{mbid}
```
