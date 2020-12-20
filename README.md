# emacs-checksum

Package to provide a checksum engine for Emacs.

By using a Common Lisp software on top of Ironclad,
you can compare the hash between two files from inside Emacs.

## How to use

Simple call `M-x checksum`.
You need to select a hash file and the file which integrity is to be checked.

If the hash doesn't end in the correct filetype
(i.e. "file.md5.txt" instead of "file.md5") you will be
asked to also enter the spec, showing all the supported ones by Ironclad.

![working package][1]


## Installation

You need to clone the project inside "~/.emacs.d/"
and compile the Common Lisp code.

There is a Makefile, the compilation is currently working with warnings.
To compile you can simple call `make` inside the project directory.

There is a name-conflict between COMMON-LISP:DESCRIBE and UNIX-OPTS:DESCRIBE.

If asked to keep old symbols, chose to take-new symbols (option 1).

![name-conflict warning][2]

The software will sucessfully compile (with warnings) under SBCL.

Other implementations than SBCL yet to be tested.

## TODO

Rewrite everything in Elisp.

[1]: ./screenshots/emacs-checksum.gif
[2]: ./screenshots/emacs-checksum-warning.png