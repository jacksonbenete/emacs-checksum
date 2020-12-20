(defpackage :emacs-checksum/cli
  (:use :common-lisp :ironclad :sb-unix)
  (:export #:main))

(in-package :emacs-checksum/cli)

;; Install ironclad (ql:quickload "ironclad")

;;; TODO: Remove if it's of no use.
;;; This will returns a digest list for usage in elisp as a plist.
(defun make-digest-list (lista)
  (if (null lista)
      lista
      (cons (cons (string (car lista)) (car lista))
	    (make-digest-list (cdr lista)))))
;; (make-digest-list (ironclad:list-all-digests))

;;; TODO: Remove if it's of no use.
(defun cli-getenv (name &optional default)
    #+CMU
    (let ((x (assoc name ext:*environment-list*
                    :test #'string=)))
      (if x (cdr x) default))
    #-CMU
    (or
     #+Allegro (sys:getenv name)
     #+CLISP (ext:getenv name)
     #+ECL (si:getenv name)
     #+SBCL (sb-unix::posix-getenv name)
     #+LISPWORKS (lispworks:environment-variable name)
     default))

;;; It will return a list of arguments passed on command line.
;;; It returns the cdr, since the first argument is either the
;;; filename or the implementation ("sbcl" in my case)
(defun cli-args ()
  (cdr (or 
	#+SBCL *posix-argv*  
	#+LISPWORKS system:*line-arguments-list*
	#+CMU extensions:*command-line-words*
	nil)))

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

;;; Simple function to read a text file (hash file)
;;; and return it.
;;; A file content of "eab0aae59006a573755d21eb48b9b3a8  slacko-5.6-PAE.iso"
;;; will return the symbol EAB0AAE59006A573755D21EB48B9B3A8
(defun hash-load-spec-file (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (read in))))

;;; Receive a spec as a symbol (i.e. :md5), and two filenames,
;;; one for the file-object and the other containing the hash.
;;; It uses equalp because the object-generated hash will be in lower case
;;; and the read-file hash will be upper case. See hash-load-spec-file.
(defun compare-object-to-file (spec object-filename spec-filename)
  (print "Comparing hashes")
  (let ((spec-file-to-string (string (hash-load-spec-file spec-filename)))
	(object-hash-to-string (hash-object-to-string spec object-filename)))
    (if (equalp spec-file-to-string
		object-hash-to-string)
	(print "Checksum passed!")
	(print "Checksum failed."))))

;;; TODO: change this function to be the main function.
;;; it need to be callable from the command line and
;;; read the args with cli-args.
;;; Should it be compiled? Should it be called as script?
(defun checksum ()
  (compare-object-to-file
   :md5
   "/home/jack/Downloads/slacko-5.6-PAE.iso"
   "/home/jack/Downloads/slacko-5.6-PAE.iso.md5.txt"))

(defun main ()
  (print "compiled and running"))

;;; TODO: remove this later.
;; (compare-object-to-file (car (cli-args)) (cadr (cli-args)) (caddr (cli-args )))
