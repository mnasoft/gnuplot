;;;; ./src/package.lisp

(defpackage :gnuplot
  (:use #:cl)
  (:export *test-tbl*
           *test-labes-list*
           *test-hash-lables*
           )
  (:export <arc>
           <arc>-radius
           <arc>-end-angle
           <arc>-center
           <arc>-start-angle          
           )
  (:export <line>
           <line>-end
           <line>-start           
           )
  (:export <point>
           <point>-coords
           polar
           )
  (:export <circle>
           <circle>-center
           <circle>-radius
           )
  (:export <gnuplot-vector-printer>
           <gnuplot-vector-printer>-stream
           )
  (:export out
           out-vectors-to-file
           out-func-polynom-fit
           out-plot
           out-table
           )
  (:export set-*range
           set-*tics
           set-title
           set-grid
           set-m*tics
           set-polar
           set-key
           set-theta
           )
  (:export unset-polar
           unset-theta)
  (:export help-set-grid
           help-set-*range
           help-set-*tics
           help-set-m*tics
           help-set-key
           help-set-polar
           help-set-title
           help-colorspec)
  (:export make-plot
           make-int-range
           make-int-list
           make-func-polynom-fit 
           make-hash-table-lables
           )
  (:export *color-names*
           )
  (:export *point-type-open*
           *point-type-fill*
           *point-type-fill-box*
           *point-type-box*
           *point-type-box-fill*
           *point-type-all*
           )
  (:export extract
           format-n-string
           stacked-chart-data
           filter))


(in-package :gnuplot)

