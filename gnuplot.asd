;;;; gnuplot.asd

(defsystem #:gnuplot
  :description "Определяет некоторые функции для построения графиков с использованием программы gnuplot"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("mnas-hash-table" "mnas-string") ;;;; "cl-annot"
  :serial t
  :components ((:file "package")
	       (:file "point-types")
               (:file "gnuplot")
	       (:file "color-names")
	       (:file "set") 
               (:file "help")
               (:file "vectors") 	       
;;; (:file "test")
	       ))
