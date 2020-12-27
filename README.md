# emacs-checksum

Package to provide a checksum engine for Emacs.

By using a Common Lisp software on top of Ironclad,
you can compare the hash between two files from inside Emacs.

## How to use

Simple call `M-x checksum`.
You need to select a hash file and the file which integrity is to be checked.

If the hash doesn't end in the correct filetype but end as a ".txt" 
(i.e. "file.md5.txt" instead of "file.md5") the software will try to guess
the correct hash type (spec).

If the hash doesn't have a valid filetype before ".txt"
(e.g. "file.iso.txt") you will be asked to also enter the spec,
showing all the supported ones by Ironclad.

![working package][1]

The software also can be used from dired-mode.

If one file is flagged, the software will generate
the desired hash for the file.

![dired using one file][2]

If two files are flagged, the software will compare hashes.

![dired using two files][3]

The software will try to guess which file is the hash and which file
is the object. If the hash file type doesn't end as expected, a error
message will appear.

(e.g. trying to compare "file.iso" and "file.bar" doesn't work yet)

## Installation

You need to clone the project inside `~/.emacs.d/`
and compile the Common Lisp code.

There is a Makefile, the compilation is currently working with warnings.
To compile you can simple call `make` inside the project directory.

There is a name-conflict between *COMMON-LISP:DESCRIBE* and *UNIX-OPTS:DESCRIBE*.

If asked to keep old symbols, chose to `TAKE-NEW` symbols (option 1).

![name-conflict warning][2]

The software will sucessfully compile (with warnings) under SBCL.

Other implementations than SBCL yet to be tested.

If you want to use (or test) the binary from the command-line:
```
> ./checksum-cli --spec md5 --file ~/my-file.iso --hash ~/my-file.iso.md5
```

## TODO

Return information as Stream populating the buffer instead of
returning everything at once in the end.

Rewrite everything in Elisp.

Test other implementations.

[1]: ./screenshots/emacs-checksum.gif
[2]: ./screenshots/emacs-checksum-dired-single-file.gif
[3]: ./screenshots/emacs-checksum-dired.gif
