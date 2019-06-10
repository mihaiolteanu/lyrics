# lyrics
Search song lyrics or, the other way around, search songs from lyrics.

# Installation

```bash
# clone to local-projects for quickload access
git clone https://github.com/mihaiolteanu/lyrics ~/quicklisp/local-projects/lyrics

# a database is used to store the lyrics
sudo pacman -S sqlite3 
```

# Usage

```common-lisp
(ql:quickload :lyrics)
```

Search the lyrics given an artist and a song name
```common-lisp
(lyrics "lake of tears" "the homecoming")
=> "It's the way of a cosmic sailor, in a boat in the night
    But the wolves are not scaring him, he's alright
    Just the day, just the day away, I can feel it sometimes
    ..."
```

Or, search in the local database for all the songs that contain a given string. The
result is a list of artist name, song-name and the verse line that contains the
input string.
```common-lisp
(search-song "the night")
=> (("coldplay" "Cemeteries of London" "save the night time for your weeping")
    ("coldplay" "Cemeteries of London" "and the night over london lay")
    ("lake of tears" "Forever Autumn" "but the night becomes you")
    ("lake of tears" "The Homecoming" "And the night that showed them all to me?"))
```

# Details
**lyrics** uses an sqlite3 database to save all the lyrics that have been searched
and found so far. The database file, `~/cl-lyrics.db`, is created when the
library is first loaded and it is saved in the user home folder.

The following lyrics websites are currently supported and searched for lyrics:
  * makeitpersonal
  * genius
  * songlyrics
  * metrolyrics
  * musixmatch
  * azlyrics

If the song cannot be found on any of the websites, `lyrics` returns nil. Otherwise
`lyrics` returns the song lyrics and saves them in the database from where they
will be fetched on the next call. The `lyrics` function is memoized.

## Authors
Copyright (c) 2019 [Mihai Olteanu](www.mihaiolteanu.me)

Licensed under the [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html) license.
