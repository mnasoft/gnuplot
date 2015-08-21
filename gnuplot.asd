;;;; gnuplot.asd

(asdf:defsystem #:gnuplot
  :description "Describe gnuplot here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :components ((:file "package")
	       (:file "point-types")
               (:file "gnuplot")
	       (:file "test")))
