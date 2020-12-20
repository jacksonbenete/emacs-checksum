build:
	sbcl --eval "(asdf:operate 'asdf:load-op 'emacs-checksum)" \
	--eval "(sb-ext:save-lisp-and-die #P\"checksum-cli\" :toplevel #'emacs-checksum/cli:main :executable t :compression t)"