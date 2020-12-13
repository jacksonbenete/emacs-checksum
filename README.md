# emacs-checksum

Package to provide a checksum engine for Emacs.

The function `secure-hash` builtin on Emacs works only for a buffer or a string.


This package aims to provide a way to produce a checksum from an object, which
is (hopefully) of any file type.

## TODO

Create a compiled software on common lisp, on top of ironclad, to be called
from inside of emacs.
