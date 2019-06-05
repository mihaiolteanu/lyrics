;;;; lyrics.lisp
(in-package #:lyrics)

(defparameter *db*
  (sqlite:connect
   (merge-pathnames "cl-lyrics.db"
                    (user-homedir-pathname))))

(defstruct website
  ;; website name, only used for documentation purposes 
  (name)
  ;; Template used to construct the actual url. It contains the artist-name and
  ;; song-name string, respectively, which need to be replaced with the user
  ;; suplied values for artist and song name.
  (url-template)
  ;; The replacing in the url-template is done with the artist and song
  ;; name. These can contain spaces on the user side. It is the task of this
  ;; library to add the separateors. Some sites required '-' separators, other
  ;; sites remove the spaces completely.
  (separator)
  ;; After the request is sent and the response received, the lyrics need to be
  ;; extracted from the received html. Every website has different css selectors
  ;; for this job. A nil value means we don't need to do any extraction
  ;; (makeitpersonal, for example).
  (css-selector))

(defparameter *raw-websites*
  '((makeitpersonal
     "https://makeitpersonal.co/lyrics?artist=artist-name&title=song-name"
      #\- nil)
    (genius
     "https://genius.com/artist-name-song-name-lyrics"
     #\- "div.lyrics p")
    (songlyrics
     "http://www.songlyrics.com/artist-name/song-name-lyrics/"
     #\- "p#songLyricsDiv")
    (metrolyrics
     "http://www.metrolyrics.com/~song-name-lyrics-~artist-name.html"
     #\- "p.verse")
    (musixmatch
     "https://www.musixmatch.com/lyrics/artist-name/song-name"
     #\- "p.mxm-lyrics__content")
    (azlyrics
     "https://www.azlyrics.com/lyrics/artist-name/song-name.html"
     nil "div.container.main-page div.row div:nth-child(2) div:nth-of-type(5)")))

(defparameter *websites*
  (mapcar (lambda (w)
            (make-website :name (first w)
                          :url-template (second w)
                          :separator (third w)
                          :css-selector (fourth w)))
          *raw-websites*)
  "List of structures, where each structure is a lyrics website")

(defun url (website artist song)
  "Create a valid url from the website pattern. First, the artist and song
string must be prepared by replacing the spaces with the website separator and
then the final url is prepared by replacing the aritst-name and song-name in the
template."
  (declare (website website))
  (declare (string artist song))
  (let ((separator (website-separator website)))
    (if separator
        ;; Prepare the artist and song string (i.e. encode the values, basically)
        (progn (setf artist (substitute separator #\Space artist))
               (setf song (substitute separator #\Space song)))
        ;; Some sites don't have any separator (i.e. no space between words)
        (progn (setf artist (remove #\Space artist))
               (setf song (remove #\Space song)))))
  (let ((url (website-url-template website)))
    (setf url (regex-replace "artist-name" url artist))
    (setf url (regex-replace "song-name" url song))
    url))

(defun find-lyrics (website artist song)
  "See if this website hosts the lyrics for this artist and song. If yes, then
use the css selectors to extract the lyrics. If not, return nil, and move along.
Maybe other sites host them."
  (declare (website website))
  (declare (string artist song))
  (multiple-value-bind
        (content response)
      (http-request (url website artist song))
    (if (= response 200)
        (let ((query (website-css-selector website)))
          (if query
              (lquery:$ (inline (parse content)) query (text))
              ;; Some websites just don't need any parsing.
              content))
        ;; Maybe this website doesn't have the lyrics for this song
        nil)))

(defun save-lyrics-to-db (artist song lyrics)
  "Save the lyrics for future retrieval and return them."
  (execute-non-query
   *db*
   (format nil "INSERT INTO lyrics(artist,song,lyrics) VALUES(\"~a\", \"~a\", \"~a\")"
           artist song lyrics))
  lyrics)

(defun lyrics-from-db (artist song)
  "Get the lyrics from the db, if they exists. Return nil otherwise."
  (execute-single *db*
   (format nil "SELECT lyrics FROM lyrics WHERE artist=\"~a\" AND song=\"~a\""
           artist song)))

(defun search-song (lyrics)
  "Return a list of entries, where each entry is a list with the artist name,
the song name and the verse line where the lyrics appears. The artist name and
song can be used to request the full lyrics. There can be multiple entries for
the same artist/song combination."
  (let* ((songs (execute-to-list
                 *db*
                 (format nil "SELECT * from lyrics WHERE lyrics like '%~a%'"
                         lyrics)))
         (result
           ;; Create an (artist song verse-line) entry for each verse line that
           ;; matches the input lyrics
           (mappend (lambda (song)
                      (let ((matches (all-matches-as-strings
                                      (concatenate 'string ".*" lyrics ".*")
                                      (fourth song))))
                        (mapcar (lambda (match)
                                  (list (second song) (third song) match))
                                matches)))
                    songs)))
    ;; Don't return identical verse lines for the same artist and song
    (remove-duplicates result :key #'third :test #'equal)))

(defmemo lyrics (artist song)
  "Memoized lyrics search. Search the lyrics for the given artist and song
name. If the lyrics are not in the db, try and extract them from one of the
supported lyrics websites. If found, save the lyrics the db and return them. If
not found, return nil."
  (declare (string artist song))
  (if-let ((lyrics (lyrics-from-db artist song)))
    lyrics                              ;already in db
    (dolist (website *websites*)
      (let ((lyrics (find-lyrics website artist song)))
        (when lyrics
          (return (save-lyrics-to-db
                   artist song
                   ;; lquery:$ returns a vector, but if parsing is not needed,
                   ;; just a simple string is returned by find-lyrics.
                   (string-downcase     ;all-matches-as-strings doesn't have
                                        ;case-insensitive search
                    (if (simple-vector-p lyrics)
                        (aref lyrics 0)
                        lyrics)))))))))


;; (ql:quickload :lyrics)
;; (lyrics:lyrics "anathema" "thin air")
;; (lyrics:search-song "this dream")
