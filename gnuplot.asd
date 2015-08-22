;;;; gnuplot.asd

(asdf:defsystem #:gnuplot
  :description "Describe gnuplot here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:mnas-hash-table)
  :serial t
  :components ((:file "package")
	       (:file "point-types")
               (:file "gnuplot")
	       (:file "color-names")
	       (:file "test")))
