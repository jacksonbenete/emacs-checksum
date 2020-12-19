(defsystem :emacs-checksum
  :depends-on (:ironclad)
  :serial t
  :components ((:file "package")
	       (:file "checksum")))