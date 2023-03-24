;;;;vectors.lisp

(in-package :gnuplot)

(defclass <point> () 
  ((coords :accessor <point>-coords :initform  '(0.0 0.0 0.0) :initarg :coords))
  (:documentation "@begin[lang=lisp](code)
 (polar  (make-instance '<point> :coords '(20.0 10.0 0.0 ) ) (/ pi 6 ) 100.0)
@end(code)
"))

(defmethod print-object ((pt <point>) s)
  (format s "~{~A~^ ~}" (<point>-coords pt)))

(defmethod polar ((p <point>) angle distance)
  (let ((pt (make-instance
	     '<point>
	     :coords
	     (list 
	      (+ (first  (<point>-coords p)) (* (cos angle) distance))
	      (+ (second (<point>-coords p)) (* (sin angle) distance))
	      (third  (<point>-coords p))))))
    pt))

(defclass <line> ()
  ((start :accessor <line>-start :initform  (make-instance '<point> :coords '(0.0     0.0 0.0)) :initarg :start)
   (end   :accessor <line>-end   :initform  (make-instance '<point> :coords '(100.0 100.0 0.0)) :initarg :end  )))

(defclass <circle> ()
  ((center :accessor <circle>-center :initform  (make-instance '<point> :coords '(0.0 0.0 0.0)) :initarg :center)
   (radius :accessor <circle>-radius :initform  1.0                                             :initarg :radius)))

(defclass <arc> ()
  ((center      :accessor <arc>-center      :initform  (make-instance '<point> :coords '(0.0 0.0 0.0)) :initarg :center)
   (radius      :accessor <arc>-radius      :initform  1.0            :initarg :radius)
   (start-angle :accessor <arc>-start-angle :initform  0.0            :initarg :start-angle)
   (end-angle   :accessor <arc>-end-angle   :initform  pi             :initarg :end-angle)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <gnuplot-printer> ()
    ((stream :accessor <gnuplot-printer>-stream
             :initform (make-string-output-stream))))

(defclass <gnuplot-vectors-printer> (<gnuplot-printer>) ())

(defclass <gnuplot-lines-printer> (<gnuplot-printer>) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
