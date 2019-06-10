;;;; lyrics.asd

(asdf:defsystem #:lyrics
  :description "Song lyrics with local database"
  :author "Mihai Olteanu <mihai_olteanu@fastmail.fm>"
  :license  "GPLv3"
  :version "0.1"
  :depends-on (:drakma
               :plump
               :lquery
               :cl-ppcre
               :sqlite
               :alexandria
               :bordeaux-threads
               :defmemo
               :replic)
  :serial t

  :build-operation "program-op"
  :build-pathname "lyrics"
  :entry-point "lyrics::main"

  :components ((:file "package")
               (:file "lyrics")))
