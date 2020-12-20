;;; checksum.asd --- Interface for comparing hash -*- lexical-binding: t -*-

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
;;; To compile the file, first run sbcl and:
;;; (asdf:operate 'asdf:load-op 'emacs-checksum)
;;;
;;; then generate the binary
;;; (sb-ext:save-lisp-and-die #P"checksum-cli" :toplevel #'emacs-checksum/cli:main :executable t :compression t)
;;;
;;; Alternatively you can run `make` to compile using Makefile.

;;; Code:

;; -----------------------------------------------

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
