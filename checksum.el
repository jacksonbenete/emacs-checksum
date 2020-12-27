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

;; -----------------------------------------------


(defgroup checksum nil
  "Compare hash of a file."
  :group 'applications)

(defvar checksum-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "Q") 'kill-this-buffer)
    map)
  "Keymap for interacting with checksum application buffer, since it will be 
a read-only buffer.")

(define-derived-mode checksum-mode special-mode "Checksum"
  "Major mode for controlling process output buffer from checksum-cli.
\\<checksum-mode-map>"
  :group 'checksum
  :syntax-table nil
  :abbrev-table nil
  (buffer-disable-undo)
  (whitespace-mode -1)
  (linum-mode -1)
  (when (>= emacs-major-version 26)
    (display-line-numbers-mode -1))
  (page-break-lines-mode 1)
  (setq inhibit-startup-screen t
        buffer-read-only t
        truncate-lines t))

(defvar checksum-buffer-name "*Process: checksum-cli*"
  "Define a common buffer name to handle process call.")

(defvar checksum-select-hash-list
  '(("ADLER32" . 'ADLER32) ("BLAKE2" . 'BLAKE2) ("BLAKE2/160" . 'BLAKE2/160)
    ("BLAKE2/256" . 'BLAKE2/256) ("BLAKE2/384" . 'BLAKE2/384)
    ("BLAKE2S" . 'BLAKE2S) ("BLAKE2S/128" . 'BLAKE2S/128)
    ("BLAKE2S/160" . 'BLAKE2S/160) ("BLAKE2S/224" . 'BLAKE2S/224)
    ("CRC24" . 'CRC24) ("CRC32" . 'CRC32) ("GROESTL" . 'GROESTL)
    ("GROESTL/224" . 'GROESTL/224) ("GROESTL/256" . 'GROESTL/256)
    ("GROESTL/384" . 'GROESTL/384) ("JH" . 'JH) ("JH/224" . 'JH/224)
    ("JH/256" . 'JH/256) ("JH/384" . 'JH/384) ("KECCAK" . 'KECCAK)
    ("KECCAK/224" . 'KECCAK/224) ("KECCAK/256" . 'KECCAK/256)
    ("KECCAK/384" . 'KECCAK/384) ("KUPYNA" . 'KUPYNA) ("KUPYNA/256" . 'KUPYNA/256)
    ("MD2" . 'MD2) ("MD4" . 'MD4) ("MD5" . 'MD5) ("RIPEMD-128" . 'RIPEMD-128)
    ("RIPEMD-160" . 'RIPEMD-160) ("SHA1" . 'SHA1) ("SHA224" . 'SHA224)
    ("SHA256" . 'SHA256) ("SHA3" . 'SHA3) ("SHA3/224" . 'SHA3/224)
    ("SHA3/256" . 'SHA3/256) ("SHA3/384" . 'SHA3/384) ("SHA384" . 'SHA384)
    ("SHA512" . 'SHA512) ("SHAKE128" . 'SHAKE128) ("SHAKE256" . 'SHAKE256)
    ("SKEIN1024" . 'SKEIN1024) ("SKEIN1024/384" . 'SKEIN1024/384)
    ("SKEIN1024/512" . 'SKEIN1024/512) ("SKEIN256" . 'SKEIN256)
    ("SKEIN256/128" . 'SKEIN256/128) ("SKEIN256/160" . 'SKEIN256/160)
    ("SKEIN256/224" . 'SKEIN256/224) ("SKEIN512" . 'SKEIN512)
    ("SKEIN512/128" . 'SKEIN512/128) ("SKEIN512/160" . 'SKEIN512/160)
    ("SKEIN512/224" . 'SKEIN512/224) ("SKEIN512/256" . 'SKEIN512/256)
    ("SKEIN512/384" . 'SKEIN512/384) ("SM3" . 'SM3) ("STREEBOG" . 'STREEBOG)
    ("STREEBOG/256" . 'STREEBOG/256) ("TIGER" . 'TIGER) ("TREE-HASH" . 'TREE-HASH)
    ("WHIRLPOOL" . 'WHIRLPOOL))
    "Return a list of digests supported by ironclad.
See (ironclad:list-all-digests) for an updated list.")

(defvar checksum-supported-hash-list
  '("ADLER32" "BLAKE2" "BLAKE2/160" "BLAKE2/256" "BLAKE2/384" "BLAKE2S"
    "BLAKE2S/128" "BLAKE2S/160" "BLAKE2S/224" "CRC24" "CRC32" "GROESTL"
    "GROESTL/224" "GROESTL/256" "GROESTL/384" "JH" "JH/224" "JH/256" "JH/384"
    "KECCAK" "KECCAK/224" "KECCAK/256" "KECCAK/384" "KUPYNA" "KUPYNA/256" "MD2"
    "MD4" "MD5" "RIPEMD-128" "RIPEMD-160" "SHA1" "SHA224" "SHA256" "SHA3"
    "SHA3/224" "SHA3/256" "SHA3/384" "SHA384" "SHA512" "SHAKE128" "SHAKE256"
    "SKEIN1024" "SKEIN1024/384" "SKEIN1024/512" "SKEIN256" "SKEIN256/128"
    "SKEIN256/160" "SKEIN256/224" "SKEIN512" "SKEIN512/128" "SKEIN512/160"
    "SKEIN512/224" "SKEIN512/256" "SKEIN512/384" "SM3" "STREEBOG"
    "STREEBOG/256" "TIGER" "TREE-HASH" "WHIRLPOOL")
  "Return a list of digests supported by ironclad.
This is useful for checking if a file type is member of.
See (ironclad:list-all-digests) for an updated list.")

;;; TODO: this function isn't working yet.
;;; The function should "read-file-name" showing only supported
;;; filenames.
;;; There is a problem though, which is when a file ends
;;; in .txt instead of .hashtype.
(defun checksum-find-hash-by-type ()
  "Find the hash file filtering by supported hash types."
  (expand-file-name
   (read-file-name "Select hash: "
		   nil nil nil nil
		   (lambda (x) (member (intern (file-name-extension x))
				       checksum-supported-hash-list)))))

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

(defun checksum-cli-compare-hashes (file hash hash-type)
  "Send a file and a hash to checksum-cli to compare hashes.
Return results as a read-only help-like buffer."
  (set-process-sentinel
     (start-process
      "checksum-cli" checksum-buffer-name
      "~/.emacs.d/emacs-checksum/checksum-cli"
      "--operation" "compare-hash"
      "--spec" (symbol-name hash-type)
      "--file" file
      "--hash" hash)
     (lambda (p e) (when (= 0 (process-exit-status p))
		     (pop-to-buffer checksum-buffer-name
				    (with-current-buffer checksum-buffer-name
				      (beginning-of-buffer)
				      (checksum-mode)))))))

(defun checksum-dired-generate-hash (file)
  "Send a file object to checksum-cli to generate a hash.
Return results as a read-only help-like buffer."
    (set-process-sentinel
     (start-process
      "checksum-cli" checksum-buffer-name
      "~/.emacs.d/emacs-checksum/checksum-cli"
      "--operation" "generate-hash"
      "--spec" (symbol-name (checksum-select-hash))
      "--file" file)
     (lambda (p e) (when (= 0 (process-exit-status p))
		     (pop-to-buffer checksum-buffer-name
				    (with-current-buffer checksum-buffer-name
				      (beginning-of-buffer)
				      (checksum-mode)))))))

(defun checksum-select-hash ()
  "Select hash from list and return as symbol."
  (let* ((key (completing-read
	       "Select hash type: " checksum-select-hash-list))
	 (val (alist-get key checksum-select-hash-list nil nil #'string=))
	 (hash-type (cadr val)))
    hash-type))

(defun checksum-dired-compare-files (file-list)
  "Receive two files, an object and a hash.
Generate a hash for the object and compare hashes.
A file ending with .txt or .spec will be treated as hash."
  (let* ((first-file (car file-list))
	 (second-file (cadr file-list))
	 (first-file-type (upcase (file-name-extension first-file)))
	 (second-file-type (upcase (file-name-extension second-file)))
	 (first-fake-txt (upcase (file-name-extension (substring first-file 0 -4))))
	 (second-fake-txt (upcase (file-name-extension (substring second-file 0 -4)))))
    ;; If file type is supported (e.g. md5) call the function
    ;; If file type is txt, remove txt and test again,
    ;; If the file was <file>.md5.txt then md5 will be used,
    ;; Else, ask for the correct spec.
    (cond ((member first-file-type checksum-supported-hash-list)
	   (checksum-cli-compare-hashes second-file first-file (intern first-file-type)))
	  
	  ((string= first-file-type "TXT")
	   (if (member first-fake-txt checksum-supported-hash-list)
	       (checksum-cli-compare-hashes second-file first-file (intern first-fake-txt))
	     (checksum-cli-compare-hashes second-file first-file (checksum-select-hash))))

	  ((member second-file-type checksum-supported-hash-list)
	   (checksum-cli-compare-hashes first-file second-file (intern second-file-type)))
	  
	  ((string= second-file-type "TXT")
	   (if (member second-fake-txt checksum-supported-hash-list)
	       (checksum-cli-compare-hashes first-file second-file (intern second-fake-txt))
	     (checksum-cli-compare-hashes first-file second-file (checksum-select-hash))))
	  
	  (t (message "Error: Hash file must end as *.spec or *.txt, 
where `spec` is one of the supported types.")))))

(defun checksum ()
  "Compare hash between two files and show results in a help buffer."
  (interactive)
  (let* ((file (checksum-find-file))
	 (hash (checksum-find-hash))
	 (hash-type (upcase (file-name-extension hash)))
	 (hash-type-fake-txt
	  (upcase (file-name-extension (substring hash 0 -4)))))
    ;; If file-type is supported, call the function
    ;; Else, if TXT, test if can ignore TXT or ask for correct spec.
    (if (member hash-type checksum-supported-hash-list)
	(checksum-cli-compare-hashes file hash (intern hash-type))
      (if (and (string= hash-type "TXT")
	       (member hash-type-fake-txt checksum-supported-hash-list))
	  (checksum-cli-compare-hashes file hash (intern hash-type-fake-txt))
	(checksum-cli-compare-hashes file hash (checksum-select-hash))))))

;;; TODO: handle multiple files
(defun checksum-dired ()
  "Compare or generate hash between files using dired."
  (interactive)
  (let* ((file-list (dired-get-marked-files))
	 (file-list-length (length file-list)))
    (if (eq major-mode 'dired-mode)
	(cond ((= file-list-length 1)
	       (checksum-dired-generate-hash (car file-list)))
	      ((= file-list-length 2)
	       (checksum-dired-compare-files file-list))
	      ((> file-list-length 2)
	       (message "Error: Multiple files not supported yet."))
	      (t (message "Error: There is no file selected.")))
      (message "Error: You're not in dired-mode."))))

(provide 'checksum)
;;; end of file checksum.el