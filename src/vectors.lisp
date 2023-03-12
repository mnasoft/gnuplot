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

(defclass <gnuplot-vector-printer> ()
  ((stream :accessor <gnuplot-vector-printer>-stream
	   :initform (make-string-output-stream) )))

(defgeneric out (object printer)
 (:documentation "@b(Описание:) out выводит объект object на принтер printer."))

(defmethod out ((l <line>) (pr <gnuplot-vector-printer>))
  (format (<gnuplot-vector-printer>-stream pr)
	  "~{~A~^ ~} ~{~A~^ ~}~%"
	  (mapcar #'(lambda (el) (coerce el 'single-float)) (<point>-coords (<line>-start l)))
	  (mapcar #'(lambda (el) (coerce el 'single-float))
		  (mapcar #'- (<point>-coords (<line>-end l)) (<point>-coords(<line>-start l))))))

(defmethod out ((ar <arc>) (pr <gnuplot-vector-printer>))
  (let* ((a-r (<arc>-radius ar))
	 (a-s (<arc>-start-angle ar))
	 (a-e (if (> (- (<arc>-end-angle ar) (<arc>-start-angle ar)) 0.0)
		  (<arc>-end-angle ar)
		  (+ (* 2 pi)(<arc>-end-angle ar))))
	 (steps (ceiling (* 100 (/ (- a-e a-s) pi 2))))
	(pts (loop :for i :from 0 :to steps
		   :collect
		   (polar
		    (<arc>-center ar)
		    (+ a-s (* i (- a-e a-s) (/ steps))) a-r))))
    (map nil
	 #'(lambda (p-s p-e)
	     (out (make-instance '<line> :start p-s :end p-e) pr))
	 pts (cdr pts))))

(defmethod out ((ar <circle>) (pr <gnuplot-vector-printer>))
  (let* ((steps 50)
	(pts (loop :for i :from 0 :to steps
		   :collect (polar
			     (<circle>-center ar)
			     (* pi 2 (/ steps) i)
			     (<circle>-radius ar)))))
    (map nil
	 #'(lambda (p-s p-e)
	     (out (make-instance '<line> :start p-s :end p-e) pr))
	 pts (cdr pts))))

(defun out-vectors-to-file (f-name sequence)
  "@b(Описание:) out-vectors-to-file выполняет вывод объектов,
   находящихся в последовательности sequence в файл с именем f-name."
  (assert 
   (every #'(lambda (el)
	      (or (eq (class-of el) (find-class '<line>))
		  (eq (class-of el) (find-class '<arc>))
		  (eq (class-of el) (find-class '<circle>))))
	  sequence))
  (with-open-file (os f-name :direction :output :if-exists :supersede)
    (let ((vector-printer (make-instance '<gnuplot-vector-printer>)))
      (map nil #'(lambda (el) (out el vector-printer)) sequence)
      (format os "~A" (get-output-stream-string
		       (<gnuplot-vector-printer>-stream vector-printer))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
