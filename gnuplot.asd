;;;; gnuplot.asd

(defsystem #:gnuplot
  :description "Describe gnuplot here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on (#:mnas-hash-table)
  :serial t
  :components ((:file "package")
	       (:file "point-types")
               (:file "gnuplot")
	       (:file "color-names")
;;; (:file "test")
	       ))
