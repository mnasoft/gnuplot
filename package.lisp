;;;; package.lisp

(defpackage #:gnuplot
  (:use #:cl)
  (:export
   *point-type-open* 
   *point-type-box*
   *point-type-fill*
   *point-type-box-fill*
   *point-type-fill-box*
   *point-type-all*
   )
  (:export
   *tbl*
   *tbl-labes-hash*
   )
  (:export
   make-hash-table-lables
   format-n-string
   extract
   out-table
   filter
   make-int-list
   make-int-range
   make-func-polynom-fit
   out-func-polynom-fit
   out-plot
   make-plot
   )
  )
