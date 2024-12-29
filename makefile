EMACS ?= emacsclient

.PHONY:	clean
clean:
	find . -name "*.elc" -delete

compile:
	$(EMACS) --eval '(mapcar `byte-compile-file (file-expand-wildcards "lisp/*.el"))'
