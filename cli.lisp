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
  (:name :operation
   :description "Flag for type of operation to be performed.
\nThe flags are: compare-hash; generate-hash; generate-hash-multiple-files."
   :long "operation"
   :short #\o
   :arg-parser #'identity)
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

;;; TODO: (sb-posix:getpid) is SBCL only.
;;; Need to handle other implementations.
;;; Return current system time of execution and Process PID.
(defun get-process-information ()
  "Returns current date as a string."
  (multiple-value-bind (seconds
			minutes
			hours
			day
			month
			year
			day-of-week
			daylight-standard-time-p
			timezone)
                       (get-decoded-time)
    (declare (ignore day-of-week daylight-standard-time-p timezone))
    (format t "~%Checksum executed at ~4,'0d-~2,'0d-~2,'0d, ~2,'0d:~2,'0d:~2,'0d~%" year month day hours minutes seconds)
    (format t "Process PID: ~s~2%" (sb-posix:getpid))))

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

;;; Receive one object and an optional spec parameter, and
;;; generate a hash for the desired spec.
;;; The Default spec is SHA256 when no spec parameter is found.
(defun checksum-generate-hash ()
  (multiple-value-bind (options)
      (unix-opts:get-opts)
    (let* ((file (getf options :file))
	   (spec (getf options :spec))
	   (spec-symbol (values (intern (string-upcase spec) "KEYWORD")))
	   (generated-hash (hash-object-to-string
			    (if spec spec-symbol :SHA256)
			    file)))
      (if file
	  (progn (format t "Generating hash...~2%")
		 (format t "Generated object hash  : ~a~2%" generated-hash))
	  (progn
	    (format t "ERROR: Flag --file is missing.~&")
	    (uiop:quit))))))

;;; Compare hash between two files.
;;; If one of the three needed parameters is missing, 
;;; the function will fail.
(defun checksum-compare-hash ()
  (multiple-value-bind (options)
      (unix-opts:get-opts)
    (let ((file (getf options :file))
	  (hash (getf options :hash))
	  (spec (getf options :spec)))
      (cond ((not file)
	     (format t "ERROR: Flag --file is missing.~&")
	     (uiop:quit))
	    ((not hash)
	     (format t "ERROR: Flag --hash is missing.~&")
	     (uiop:quit))
	    ((not spec)
	     (format t "ERROR: Flag --spec is missing.~&")
	     (uiop:quit))
	    (t (format t "Comparing hashes...~2%")
	       (compare-object-to-file spec file hash))))))

;;; TODO: Implement a case where a lot of files (free-args)
;;; are given.
;;; Generate a hash for each one of them.
(defun checksum-generate-hash-multiple-files ()
  ;; (multiple-value-bind (options free-args))
  (format t "ERROR: Yet to be done.")
  (uiop:quit))

;;; Call the correct function for each type of operation
;;; received on --operation flag.
(defun checksum-operation-handler ()
  (multiple-value-bind (options)
      (unix-opts:get-opts)
    (let ((operation (getf options :operation)))
      (get-process-information)
      (cond ((equal operation "generate-hash")
	     (checksum-generate-hash))
	    ((equal operation "compare-hash")
	     (checksum-compare-hash))
	    ((equal operation "generate-hash-multiple-files")
	     (checksum-generate-hash-multiple-files))
	    (t (format t "ERROR: Require operation doesn't exists.")
	       (uiop:quit))))))

(defun main ()
  (multiple-value-bind (options)
      ;; handle errors
      (handler-case
	  (unix-opts:get-opts)
	(unix-opts:missing-arg (condition)
	  (format t "ERROR: Argument of ~s is missing.~&"
		  (unix-opts:option condition))
	  (uiop:quit))
	(unix-opts:unknown-option (condition)
	  (format t "ERROR: Unknown option ~s provided.~&"
		  condition)
	  (uiop:quit)))
    ;; parse options
    (if (getf options :help)
        (progn
          (opts:describe
           :prefix "Checksum-cli. \\nUsage: cli.lisp -s md5 -f \"~/my-file.iso\" -H \"~/my-hash.md5\""
           :args "[keywords]")
          (uiop:quit)))
    (if (getf options :operation)
	(checksum-operation-handler)
	(progn
	  (format t "Operation flag is missing.~&")
	  (uiop:quit)))))

;;; END OF FILE