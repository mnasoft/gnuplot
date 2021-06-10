;;;; package.lisp

(defpackage #:gnuplot
  (:use #:cl)
  (:export
   *test-tbl*
   *test-labes-list*
   *test-hash-lables*
   )
  (:export make-plot extract
         set-*range
         *point-type-open*
         help-set-grid
         *point-type-fill-box*
         format-n-string
         <arc>-radius <line>
         set-*tics
         stacked-chart-data
         make-int-range
         *point-type-box*
         help-set-*range
         set-title
         set-grid
         <point>-coords
          <gnuplot-vector-printer>
         out-func-polynom-fit
         *color-names*
         out-plot
         <arc>-end-angle
         set-m*tics
         out
         help-set-*tics
         *point-type-all*
         out-table
         *point-type-fill*
         <arc>
         out-vectors-to-file
         <circle>-radius
         help-set-m*tics
         <arc>-center
          make-int-list <arc>-start-angle
          <circle> filter <circle>-center
          set-polar help-set-key
          help-set-polar help-set-title
          help-colorspec <point>
          make-func-polynom-fit set-key
          <line>-end <line>-start
          <gnuplot-vector-printer>-stream set-theta
          polar *point-type-box-fill*
          make-hash-table-lables
          )
  )

;;;; (declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))
;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))
