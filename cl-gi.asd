(asdf:defsystem :cl-gi
  :version "0.1"
  :author "Matt Novenstern <fisxoj@gmail.com>"
  :license "GPLv3"
  :serial t
  :components ((:file "package")
	       (:file "translator")
	       (:module src
			:components ((:file "util")
				     (:file "translators")
				     (:file "includes")
				     (:file "girepository")
				     (:file "constant")
				     (:file "enum"))))
  :depends-on ("cffi" "split-sequence"))
