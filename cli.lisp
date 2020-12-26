;;; cli.lisp --- Interface for comparing hash -*- lexical-binding: t -*-

;; Author: Jackson Benete Ferreira <jacksonbenete@gmail.com>
;; URL: https://github.com/jacksonbenete/emacs-checksum.el
;; Package-Version: 0.1
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
;;; This code provides a cli software written in Common Lisp
;;; to be used from elisp.

;;; Code:

;; -----------------------------------------------


(defpackage :emacs-checksum/cli
  (:use :common-lisp)
  (:export #:main))

(in-package :emacs-checksum/cli)


;;; Command Line Arguments
;; -----------------------------------------------
(unix-opts:define-opts
  (:name :help
   :description "Print this help text"
   :short #\h
   :long "help")
  (:name :spec
   :description "Select the (i.e. md5, sha256, ...)"
   :short #\s
   :long "spec"
   :arg-parser #'identity)
  (:name :file
   :description "Pathname for the file object."
   :short #\f
   :long "file"
   :arg-parser #'identity)
  (:name :hash
   :description "Pathname for the hash file."
   :short #\H
   :long "hash"
   :arg-parser #'identity))
;; -----------------------------------------------


;;; TODO: Remove if it's of no use.
;;; This will returns a digest list for usage in elisp as a plist.
(defun make-digest-list (lista)
  (if (null lista)
      lista
      (cons (cons (string (car lista)) (car lista))
	    (make-digest-list (cdr lista)))))
;; (make-digest-list (ironclad:list-all-digests))

;;; digest-file receive a symbol (i.e. :md5) and a filename
;;; and returns a "digest", which is an vector (SIMPLE-ARRAY (UNSIGNED-BYTE 8))
(defun hash-object (spec file-pathname)
  (ironclad:digest-file spec file-pathname))

;;; In this case, the returned vector is passed as argument
;;; to byte-array-to-hex-string, which receives a vector
;;; and return a string
(defun hash-object-to-string (spec file-pathname)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-file spec file-pathname)))

;;; TODO: Check if compiler function uiop:read-file-string
;;; is available for all implementations.
;;;
;;; Read a string using compiler function.
(defun hash-load-spec-file (filename)
  (car (split-sequence:split-sequence
	#\Space
	(uiop:read-file-string filename))))

;;; Receive a spec as a symbol (i.e. :md5), and two filenames,
;;; one for the file-object and the other containing the hash.
;;; It uses equalp because the object-generated hash will be in lower case
;;; and the read-file hash will be upper case. See hash-load-spec-file.
(defun compare-object-to-file (spec object-filename spec-filename)
  (let ((spec-file-to-string (string (hash-load-spec-file spec-filename)))
	(object-hash-to-string (hash-object-to-string
				(values (intern (string-upcase spec) "KEYWORD"))
				object-filename)))
    (format t "Generated object hash  : ~a~%" object-hash-to-string)
    (format t "Received hash from file: ~a~2%" spec-file-to-string)
    (if (equal spec-file-to-string
		object-hash-to-string)
	(format t "Checksum matches.~%")
	(format t "Checksum failed.~%"))))

(defun main ()
  (defvar spec-var)
  (defvar file-var)
  (defvar hash-var)
  ;; TODO: Implement condidion-handling
  (multiple-value-bind (options free-args)
      (unix-opts:get-opts)
    (if (getf options :help)
        (progn
          (opts:describe
           :prefix "Checksum-cli. \nUsage: cli.lisp -s md5 -f \"~/my-file.iso\" -H \"~/my-hash.md5\""
           :args "[keywords]")
          (uiop:quit)))
    (if (getf options :spec)
	(setf spec-var (getf options :spec))
	(and (print "Wainting for spec") (uiop:quit)))
    (if (getf options :file)
	(setf file-var (getf options :file))
	(and (print "Wainting for file pathname") (uiop:quit)))
    (if (getf options :hash)
	(setf hash-var (getf options :hash))
	(and (print "Wainting for hash pathname") (uiop:quit)))
    (format t "Comparing hashes...~2%")
    (compare-object-to-file spec-var file-var hash-var)
    ))

;;; END OF FILE