;;; checksum.el --- Interface for comparing hash -*- lexical-binding: t -*-

;; Author: Jackson Benete Ferreira <jacksonbenete@gmail.com>
;; URL: https://github.com/jacksonbenete/emacs-checksum.el
;; Package-Version: xx
;; Package-Commit: xx
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))

;; This file is NOT part of GNU Emacs.

;; The MIT License (MIT)
;;
;; Copyright (c) 2016 Al Scott
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;; Select your favorite mode or buffer from a pre-defined list.
;; This way you can avoid defining too much keybindings and you can have a list
;; of useful modes always in hand/

;;; Code:

;; -------------------------------------------------------------------


(defgroup checksum nil
  "Compare hash of a file."
  :group 'emacs)

(defvar checksum-select-hash-list
  "Return a list of valid hash types.
See the function (secure-hash-algorithms) for an updated list."
  '(("md5" . 'md5)
    ("sha1" . 'sha1)
    ("sha224" . 'sha224)
    ("sha256" . 'sha256)
    ("sha384" . 'sha384)
    ("sha512" . 'sha512)))

;; TODO: correct function
(defun checksum-find-hash-by-type ()
  "Find the hash file filtering by supported hash types."
  (expand-file-name
   (read-file-name "Select hash: "
		   nil nil nil nil
		   (lambda (x) (member (intern (file-name-extension x))
				       (secure-hash-algorithms))))))
;; (checksum-find-hash-by-type)

(defun checksum-find-hash ()
  "Find the hash file."
  (expand-file-name
   (read-file-name "Select hash: ")))

(defun checksum-find-file ()
  "Find the object file."
  (expand-file-name
   (read-file-name "Select file: ")))

(defun get-string-from-file (file-path)
  "Return file content.
http://ergoemacs.org/emacs/elisp_read_file_content.html"
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

;; TODO: for now, it only checksum TEXT and BUFFER
;; need to use ironclad on common-lisp, create a simple checksum in clisp
;; compile it, to use it on elisp calling a shell command
;; http://ergoemacs.org/emacs/elisp_call_shell_command.html
;; https://www.cliki.net/Ironclad
;; https://sinax.be/blog/lisp/getting-started-with-asdf.html
(defun checksum ()
  "Compare a hash and a object to checksum."
  (interactive)
  ;; Get hash, or ask for the hash type if selected file doesn't have a type.
  (setq hash (checksum-find-hash))
  (setq hash-type (intern (file-name-extension hash)))
  (unless (member hash-type (secure-hash-algorithms))
    (let* ((key (completing-read "Select hash type: " checksum-select-hash-list))
	   (val (alist-get key checksum-select-hash-list nil nil #'string=)))
      (setq hash-type (cadr val))))
  ;; Get the file object to be compared
  (setq file (checksum-find-file))
  (if (string= (secure-hash hash-type file)
	       (get-string-from-file hash))
      (message "Checksum PASSED")
    (message "Checksum FAILED")))

(provide 'checksum)
;;; end of file checksum.el