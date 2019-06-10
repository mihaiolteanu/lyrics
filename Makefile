LISP?=sbcl

build:
	$(LISP)	--non-interactive \
		--load lyrics.asd \
		--eval '(ql:quickload :lyrics)' \
		--eval '(asdf:make :lyrics)'
