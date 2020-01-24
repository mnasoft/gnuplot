;;;; package.lisp

(defpackage #:gnuplot
  (:use #:cl)
  (:export
   *test-tbl*
   *test-labes-list*
   *test-hash-lables*
   )
  )

;;;; (declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))
;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))
