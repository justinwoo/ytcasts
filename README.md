# ytcasts

[![Build Status](https://travis-ci.org/justinwoo/ytcasts.svg?branch=master)](https://travis-ci.org/justinwoo/ytcasts)

Because I wanted to download "YoutubeCasts" using youtube-dl without much effort.

Will utterly fail if/when Youtube changes their video page, but whatever. This was easy.

## Installation

Needs youtube-dl, maybe ffmpeg?

Needs a config.json with "targets" with links for Youtube channel video pages. Like so:

```ini
[ytcasts]
channels=highnote0000
limit=20
```
