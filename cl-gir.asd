(defsystem cl-gir
  :version "0.1"
  :author "Matt Novenstern <fisxoj@gmail.com>"
  :license "GPLv3"
  :components ((:file "package")
	       (:module repository
			:pathname "repository"
			:components ((:file "repository")
				     (:file "finder")
				     (:file "reader"))))
  :depends-on ("cffi" "alexandria" "xmls" "split-sequence"))