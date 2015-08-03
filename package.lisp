;;;; package.lisp

(defpackage #:gnuplot
  (:use #:cl)
  (:export *point-type-clear*
	   *point-type-fill*
	   *point-type*
	   make-hash-table-lables
	   format-n-string
	   extract
	   out-list
	   filter
	   make-int-list
	   make-int-range
	   make-func-polynom-fit
	   out-func-polynom-fit
	   out-plot
	   make-plot
	   )
  )

