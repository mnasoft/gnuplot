;;;; test.lisp

(in-package :gnuplot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *test-tbl*
  '((0 0.0 0.0 0.0 5.0)
    (1 1.0 0.1 0.01 9.207355)
    (2 1.4142135 0.4 0.08 9.546487)
    (3 1.7320508 0.90000004 0.26999998 5.7056)
    (4 2.0 1.6 0.64 1.2159874)
    (5 2.236068 2.5 1.2499999 0.20537853)
    (6 2.4494898 3.6000001 2.1599998 3.6029224)
    (7 2.6457512 4.9 3.43 8.284933)
    (8 2.828427 6.4 5.12 9.946791)
    (9 3.0 8.1 7.2899995 7.0605927))
  "Содержит таблицу для построения тестовых графиков.")

(defparameter  *test-labes-list*
  '((0 "t_1" "s")
    (1 "S_1" "m")
    (2 "V_1" "m/s")
    (3 "A_1" "m/(s^2)")
    (4 "Y_1" "m/(s^3)"))
  "Содержит тестовый набор меток.")

(defparameter  *test-hash-lables* (make-hash-table-lables *test-labes-list*)
 "Пример хеш таблицы, содержащей описания имен параметров." )

;;;;(make-plot *test-tbl* *test-hash-lables* :x1y1 '(0 (2 3)) :x1y2 '(0 (1 4)))

*test-tbl*
*test-labes-list*
*test-hash-lables*

(make-plot
 '((1 1 1 1 1 1)
   (2 4 4 4 4 4)
   (3 9 9 9 9 9)
   (4 16 16 16 16))
 (make-hash-table-lables
  '((0 "Q" "kW")
    (1 "t_1" "K")
    (2 "t_2" "K")
    (3 "t_3" "K")
    (4 "t_4" "K")
    (5 "t_5" "K")))
 :x1y1 '(0 (1 2 3 4 5))
 :stepen 2  
 )

(require :lst-arr)
(defparameter *pressure-r*
  (lst-arr:transpose
   '((25000	20000	15000	10000	9080	8540	5000	2500	0)
     (1.46      1.49	1.52	1.56	1.57	1.91	2.03	2.19	1.72)
     (2.11      2.14	2.18	2.22	2.24	2.14	2.19	2.23	2.42)
     (3.54      3.60	3.67	3.75	3.78	4.01	4.17	4.38	4.09)))
  "Относительные потери давления")

(setf *pressure-r*
      (mapcar
       #'(lambda (el)
	   (setf (first el) (/ (first el) 25000.0))
	   el)
       *pressure-r*))


(make-plot
 *pressure-r*
 (make-hash-table-lables
  '((0 "Ne" "kW")
    (1 "r_{diff-mtr}" "K")
    (2 "r_{mtr-ks-out}" "K")
    (3 "r_{ks}" "K")))
 :x1y1 '(0 (1 2 3 ))
 :stepen 2  
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (defparameter *l*
    (make-instance '<line>
		   :start (make-instance '<point> :coords '(10.0 10.0 0.0))
		   :end   (make-instance '<point> :coords '(25.0 15.0 0.0))))
  (defparameter *ar*
    (make-instance '<arc>
		   :center (make-instance '<point>
					  :coords '(10.0 50.0 0.0))
		   :radius 10.0
		   :start-angle (* 10/8 pi)
		   :end-angle   (* 4/8 pi )))
  
  (defparameter *cr*
    (make-instance '<circle>
		   :center (make-instance '<point>
					  :coords '(10.0 50.0 0.0)) :radius 10.0))
  (defparameter *s* (make-instance '<gnuplot-printer>))
  (out  *ar* *s*)
  (out  *cr* *s*)
  (out *l* *s*)

  (format t "~A"(get-output-stream-string (<gnuplot-printer>-stream *s*)))
  (out-vectors-to-file "~/data.data" (list *l* *l* *l* *ar* *cr*))
  )
