# Build

```
cabal install exe:func-ytbz --install-method=copy --overwrite-policy=always --installdir=dist
```

# Usage with youtube-dl

```
./func-ytbz a17a48b6-51db-3c52-8fdd-066fb9989f78 | parallel -j 8 youtube-dl -x --audio-format best --audio-quality 0 --output "'%(artist)s/%(album)s/%(track)s.%(ext)s'"
```