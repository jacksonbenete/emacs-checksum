(defpackage :emacs-checksum/cli
  (:use :common-lisp :ironclad :unix-opts)
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
  (let ((spec-file-to-string (string (hash-load-spec-file spec-filename)))
	(object-hash-to-string (hash-object-to-string
				(values (intern (string-upcase spec) "KEYWORD"))
				object-filename)))
    (if (equalp spec-file-to-string
		object-hash-to-string)
	(print "Checksum passed!")
	(print "Checksum failed."))))

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
    (format t "Comparing hashes...~%")
    (compare-object-to-file spec-var file-var hash-var)
    ))

;;; END OF FILE