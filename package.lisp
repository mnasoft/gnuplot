;;;; package.lisp

(defpackage #:gnuplot)

(defpackage #:gnuplot
  (:use #:cl)
  (:export
   gnuplot::*point-type-open* 
   gnuplot::*point-type-box*
   gnuplot::*point-type-fill*
   gnuplot::*point-type-box-fill*
   gnuplot::*point-type-fill-box*
   gnuplot::*point-type-all*
   )
  (:export
   gnuplot::*color-names*
   )
  (:export
   gnuplot::*test-tbl*
   gnuplot::*test-labes-list*
   gnuplot::*test-hash-lables*
   )
  (:export
   gnuplot::make-hash-table-lables
   gnuplot::format-n-string
   gnuplot::extract
   gnuplot::out-table
   gnuplot::filter
   gnuplot::make-int-list
   gnuplot::make-int-range
   gnuplot::make-func-polynom-fit
   gnuplot::out-func-polynom-fit
   gnuplot::out-plot
   gnuplot::make-plot
   )
  )

;;;;(declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))

;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))
