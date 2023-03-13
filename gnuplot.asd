;;;; gnuplot.asd

(defsystem "gnuplot"
  :description "Определяет некоторые функции для построения графиков с
  использованием программы gnuplot."
  :version "0.0.2"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("mnas-hash-table" "mnas-string") ;;;; "cl-annot"
  :serial nil
  :components ((:module "src"
                :serial nil
                :components ((:file "package")
	                     (:file "point-types")
                             (:file "gnuplot")
	                     (:file "color-names")
	                     (:file "set") 
                             (:file "help")
                             (:file "vectors")
                             #+ni (:file "test")
                             ))
               (:module "src/methods"
                :depends-on ("src")
                :serial nil
                :components ((:file "out") 	       
                             ))))
