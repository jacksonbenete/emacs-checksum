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

Flag the desired files and call `checksum-dired`.

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

There is a Makefile.

To compile you can simple call `make` inside the project directory.

The software will sucessfully compile under SBCL.

Other implementations than SBCL yet to be tested.

## Using the binary

If you want to use (or test) the binary from the command-line:
```
> ./checksum-cli --operation compare-hash --spec md5 --file ~/my-file.iso --hash ~/my-file.iso.md5
```

The software will not run without the --operation flag,
because it handles different types of operations in one or multiple files.

The current flags working are:
- `--operation compare-hash` for comparing a file with a hash.
- `--operation generate-hash` for generating a hash for a single file.

You will need additional flags depending on the operation type.

The possible additional flags are: `--spec`, `--file`, `--hash`.

You can also call `--help` for more information.

## TODO

Return information as Stream populating the buffer instead of
returning everything at once in the end.

Rewrite everything in Elisp.

Test other implementations.

[1]: ./screenshots/emacs-checksum.gif
[2]: ./screenshots/emacs-checksum-dired-single-file.gif
[3]: ./screenshots/emacs-checksum-dired.gif
