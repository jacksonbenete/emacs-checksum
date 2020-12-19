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
  '(("ADLER32" . :ADLER32) ("BLAKE2" . :BLAKE2) ("BLAKE2/160" . :BLAKE2/160)
    ("BLAKE2/256" . :BLAKE2/256) ("BLAKE2/384" . :BLAKE2/384)
    ("BLAKE2S" . :BLAKE2S) ("BLAKE2S/128" . :BLAKE2S/128)
    ("BLAKE2S/160" . :BLAKE2S/160) ("BLAKE2S/224" . :BLAKE2S/224)
    ("CRC24" . :CRC24) ("CRC32" . :CRC32) ("GROESTL" . :GROESTL)
    ("GROESTL/224" . :GROESTL/224) ("GROESTL/256" . :GROESTL/256)
    ("GROESTL/384" . :GROESTL/384) ("JH" . :JH) ("JH/224" . :JH/224)
    ("JH/256" . :JH/256) ("JH/384" . :JH/384) ("KECCAK" . :KECCAK)
    ("KECCAK/224" . :KECCAK/224) ("KECCAK/256" . :KECCAK/256)
    ("KECCAK/384" . :KECCAK/384) ("KUPYNA" . :KUPYNA) ("KUPYNA/256" . :KUPYNA/256)
    ("MD2" . :MD2) ("MD4" . :MD4) ("MD5" . :MD5) ("RIPEMD-128" . :RIPEMD-128)
    ("RIPEMD-160" . :RIPEMD-160) ("SHA1" . :SHA1) ("SHA224" . :SHA224)
    ("SHA256" . :SHA256) ("SHA3" . :SHA3) ("SHA3/224" . :SHA3/224)
    ("SHA3/256" . :SHA3/256) ("SHA3/384" . :SHA3/384) ("SHA384" . :SHA384)
    ("SHA512" . :SHA512) ("SHAKE128" . :SHAKE128) ("SHAKE256" . :SHAKE256)
    ("SKEIN1024" . :SKEIN1024) ("SKEIN1024/384" . :SKEIN1024/384)
    ("SKEIN1024/512" . :SKEIN1024/512) ("SKEIN256" . :SKEIN256)
    ("SKEIN256/128" . :SKEIN256/128) ("SKEIN256/160" . :SKEIN256/160)
    ("SKEIN256/224" . :SKEIN256/224) ("SKEIN512" . :SKEIN512)
    ("SKEIN512/128" . :SKEIN512/128) ("SKEIN512/160" . :SKEIN512/160)
    ("SKEIN512/224" . :SKEIN512/224) ("SKEIN512/256" . :SKEIN512/256)
    ("SKEIN512/384" . :SKEIN512/384) ("SM3" . :SM3) ("STREEBOG" . :STREEBOG)
    ("STREEBOG/256" . :STREEBOG/256) ("TIGER" . :TIGER) ("TREE-HASH" . :TREE-HASH)
    ("WHIRLPOOL" . :WHIRLPOOL))
      "Return a list of digests supported by ironclad.
See (ironclad:list-all-digests) for an updated list.")

;;; TODO: Remove if it's of no use.
;; (defvar checksum-select-hash-list
;;   '(("ADLER32" . 'ADLER32) ("BLAKE2" . 'BLAKE2) ("BLAKE2/160" . 'BLAKE2/160)
;;     ("BLAKE2/256" . 'BLAKE2/256) ("BLAKE2/384" . 'BLAKE2/384)
;;     ("BLAKE2S" . 'BLAKE2S) ("BLAKE2S/128" . 'BLAKE2S/128)
;;     ("BLAKE2S/160" . 'BLAKE2S/160) ("BLAKE2S/224" . 'BLAKE2S/224)
;;     ("CRC24" . 'CRC24) ("CRC32" . 'CRC32) ("GROESTL" . 'GROESTL)
;;     ("GROESTL/224" . 'GROESTL/224) ("GROESTL/256" . 'GROESTL/256)
;;     ("GROESTL/384" . 'GROESTL/384) ("JH" . 'JH) ("JH/224" . 'JH/224)
;;     ("JH/256" . 'JH/256) ("JH/384" . 'JH/384) ("KECCAK" . 'KECCAK)
;;     ("KECCAK/224" . 'KECCAK/224) ("KECCAK/256" . 'KECCAK/256)
;;     ("KECCAK/384" . 'KECCAK/384) ("KUPYNA" . 'KUPYNA) ("KUPYNA/256" . 'KUPYNA/256)
;;     ("MD2" . 'MD2) ("MD4" . 'MD4) ("MD5" . 'MD5) ("RIPEMD-128" . 'RIPEMD-128)
;;     ("RIPEMD-160" . 'RIPEMD-160) ("SHA1" . 'SHA1) ("SHA224" . 'SHA224)
;;     ("SHA256" . 'SHA256) ("SHA3" . 'SHA3) ("SHA3/224" . 'SHA3/224)
;;     ("SHA3/256" . 'SHA3/256) ("SHA3/384" . 'SHA3/384) ("SHA384" . 'SHA384)
;;     ("SHA512" . 'SHA512) ("SHAKE128" . 'SHAKE128) ("SHAKE256" . 'SHAKE256)
;;     ("SKEIN1024" . 'SKEIN1024) ("SKEIN1024/384" . 'SKEIN1024/384)
;;     ("SKEIN1024/512" . 'SKEIN1024/512) ("SKEIN256" . 'SKEIN256)
;;     ("SKEIN256/128" . 'SKEIN256/128) ("SKEIN256/160" . 'SKEIN256/160)
;;     ("SKEIN256/224" . 'SKEIN256/224) ("SKEIN512" . 'SKEIN512)
;;     ("SKEIN512/128" . 'SKEIN512/128) ("SKEIN512/160" . 'SKEIN512/160)
;;     ("SKEIN512/224" . 'SKEIN512/224) ("SKEIN512/256" . 'SKEIN512/256)
;;     ("SKEIN512/384" . 'SKEIN512/384) ("SM3" . 'SM3) ("STREEBOG" . 'STREEBOG)
;;     ("STREEBOG/256" . 'STREEBOG/256) ("TIGER" . 'TIGER) ("TREE-HASH" . 'TREE-HASH)
;;     ("WHIRLPOOL" . 'WHIRLPOOL))
;;     "Return a list of digests supported by ironclad.
;; See (ironclad:list-all-digests) for an updated list.")

;;; TODO: remove if it's of no use.
(defun checksum-find-hash-by-type ()
  "Find the hash file filtering by supported hash types."
  (expand-file-name
   (read-file-name "Select hash: "
		   nil nil nil nil
		   (lambda (x) (member (intern (file-name-extension x))
				       (secure-hash-algorithms))))))

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

;;; TODO: for now, it only checksum TEXT and BUFFER
;;; Need to find a way to call common-lisp either compiled or as script.
;;; 
;;; See also (start-process-shell-command) at:
;;; www.gnu.org/software/emacs/manual/html_node/elisp/Asynchronous-Processes.html
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