(defsystem cl-gir
  :version "0.1"
  :author "Matt Novenstern <fisxoj@gmail.com>"
  :license "GPLv3"
  :serial t
  :components ((:file "package")
;	       (:file "repository")
;	       (:file "finder")
;	       (:file "reader")
	       (:file "translator")
	       (:module girepository
			:components ((:file "translators")
				     (:file "includes")
				     (:file "girepository")
				     ) 
			))
  :depends-on ("cffi" "split-sequence"))
