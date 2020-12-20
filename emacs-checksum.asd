;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; To compile the file, first run sbcl and:
;;; (asdf:operate 'asdf:load-op 'emacs-checksum)
;;;
;;; then generate the binary
;;; (sb-ext:save-lisp-and-die #P"checksum-cli" :toplevel #'emacs-checksum/cli:main :executable t :compression t)

(asdf:defsystem emacs-checksum
  :name "emacs-checksum"
  :version "0.1"
  :maintainer "emacs-checksum developers"
  :author "Jackson Benete <jacksonbenete@gmail.com>"
  :license "MIT"
  :description "Checksum utility written in Common Lisp"
  :long-description "Compare hashes between two files inside elips/emacs."
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:emacs-checksum/cli))
