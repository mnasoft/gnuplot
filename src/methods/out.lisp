;;;; ./src/methods/out.lisp

(in-package :gnuplot)

(defgeneric out (object printer)
 (:documentation "@b(Описание:) out выводит объект object на принтер printer."))

(defmethod out ((l <line>) (pr <gnuplot-vectors-printer>))
  (format (<gnuplot-printer>-stream pr)
	  "~{~A~^ ~} ~{~A~^ ~}~%"
	  (mapcar #'(lambda (el) (coerce el 'single-float)) (<point>-coords (<line>-start l)))
	  (mapcar #'(lambda (el) (coerce el 'single-float))
		  (mapcar #'- (<point>-coords (<line>-end l)) (<point>-coords(<line>-start l))))))

(defmethod out ((l <line>) (pr <gnuplot-lines-printer>))
  "
 @b(Пример использования:)
@begin[lang=lisp](code)
  (let ((line
          (make-instance '<line>
                         :start (make-instance '<point> :coords '(0.0 0.0 0.0))
                         :end   (make-instance '<point> :coords '(10.0 10.0 0.0))))
        (printer
          (make-instance '<gnuplot-lines-printer>)))
    (progn
      (out line printer)
      (format t \"~A\"
              (get-output-stream-string
               (<gnuplot-printer>-stream printer)))))
@end(code)
"
  (format (<gnuplot-printer>-stream pr) "~{~A~^ ~}~%"
          (mapcar
           #'(lambda (el) (coerce el 'single-float))
           (<point>-coords (<line>-start l))))
  (format (<gnuplot-printer>-stream pr) "~{~A~^ ~}~%"
  	  (mapcar #'(lambda (el) (coerce el 'single-float))
                  (<point>-coords (<line>-end l)))))

(defmethod out ((ar <arc>) (pr <gnuplot-vectors-printer>))
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

(defmethod out ((ar <arc>) (pr <gnuplot-lines-printer>))
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


(defmethod out ((ar <circle>) (pr <gnuplot-vectors-printer>))
  (let* ((steps 50)
	(pts (loop :for i :from 0 :to (+ steps 1)
		   :collect (polar
			     (<circle>-center ar)
			     (* pi 2 (/ steps) i)
			     (<circle>-radius ar)))))
    (map nil
	 #'(lambda (p-s p-e)
	     (out (make-instance '<line> :start p-s :end p-e) pr))
	 pts (cdr pts)))
    (format (<gnuplot-printer>-stream pr) "~%"))

(defmethod out ((ar <circle>) (pr <gnuplot-lines-printer>))
  (let* ((steps 50)
	(pts (loop :for i :from 0 :to steps
		   :collect (polar
			     (<circle>-center ar)
			     (* pi 2 (/ steps) i)
			     (<circle>-radius ar)))))
    (map nil
	 #'(lambda (p-s p-e)
	     (out (make-instance '<line> :start p-s :end p-e) pr))
	 pts (cdr pts)))
  (format (<gnuplot-printer>-stream pr) "~%"))



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
    (let ((vector-printer (make-instance '<gnuplot-printer>)))
      (map nil #'(lambda (el) (out el vector-printer)) sequence)
      (format os "~A" (get-output-stream-string
		       (<gnuplot-printer>-stream vector-printer))))))
