;;;; package.lisp

(defpackage #:lyrics
  (:use #:cl :drakma :sqlite :defmemo)
  (:import-from :alexandria :if-let)
  (:import-from :alexandria :mappend)
  (:import-from :cl-ppcre :regex-replace)
  (:import-from :cl-ppcre :regex-replace-all)
  (:import-from :cl-ppcre :all-matches-as-strings)
  (:import-from :plump :parse)
  (:import-from :lquery :$)
  (:export lyrics search-song))


