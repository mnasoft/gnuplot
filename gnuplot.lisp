;;;; gnuplot.lisp

(in-package #:gnuplot)

;;; "gnuplot" goes here. Hacks and glory await!

(defparameter *const-names*
  (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "w")
  "Имена переменных для использования при создании осредняющих функций 
при помощи make-func-polynom-fit")

(defparameter *point-type-open* '(1 2 3 4 6 8 10 12 14))
(defparameter *point-type-close* '(5 7 9 11 13 15))
(defparameter *point-type* '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))

(defparameter *tbl*
  '((0 0.0 0.0 0.0 5.0)
    (1 1.0 0.1 0.01 9.207355)
    (2 1.4142135 0.4 0.08 9.546487)
    (3 1.7320508 0.90000004 0.26999998 5.7056)
    (4 2.0 1.6 0.64 1.2159874)
    (5 2.236068 2.5 1.2499999 0.20537853)
    (6 2.4494898 3.6000001 2.1599998 3.6029224)
    (7 2.6457512 4.9 3.43 8.284933)
    (8 2.828427 6.4 5.12 9.946791)
    (9 3.0 8.1 7.2899995 7.0605927)))

(defparameter  *tbl-labes*
  '((0 "x_1" "мм^2")
    (1 "y_1" "s^2")
    (2 "y_2" "s^2")
    (3 "y_3"  "s^2")
    (4 "y_4"  "s^2")))

(defun make-hash-table-lables(lables)
  (let ((ht (make-hash-table)))
    (mapc #'(lambda (el) (setf (gethash (first el) ht)
			       (second el)))
	  lables)
    ht))

(defparameter  *tbl-labes-hash* (make-hash-table-lables *tbl-labes*))

(defun format-n-string(n &key (str-format "~A") (str-delimiter " "))
  "Формирует строку для использования с функцией format
для вывода n значений в формате str-format c разделителями str-delimiter
Пример использования:
(format-n-string 15)
(format-n-string 15 :str-delimiter \",\")
(format-n-string 15 :str-format \"~S\")
"
  (let ((str ""))
    (dotimes (i n (concatenate 'string (string-right-trim str-delimiter str) "~%"))
      (setf str (concatenate 'string str str-format str-delimiter)))))

(defun extract (col-number-list val-list)
  "Выбирает из списка списков val-list 
колонки с номерами, находящиеся в списке col-number-list
Возвращает список списков из отобранных колонок
Пример использования:
(extract '(0 (2 4)) 
	 '(( 0  1  2  3  4  5  6)
	   (10 11 12 13 14 15 16)
	   (20 21 22 23 24 25 26)
	   (30 31 32 33 34 35 36)))
"
  (mapcar   #'(lambda (el)
		(mapcar
		 #'(lambda (n) (nth n el))
		 (make-int-list col-number-list)))
	    val-list))

(defun out-list(lst &optional (out t) &key (str-format "~A") (str-delimiter " "))
  (mapcar
   #'(lambda (el)
       (apply #'format out (format-n-string (length el) :str-format str-format :str-delimiter str-delimiter) el))
   lst)
  t)

(defun filter(table &optional (test #'(lambda (el) t)))
  "Выполняет фильтрацию строк из таблицы по определенному критерию,
задаваемому в функции test
Функция test принимает обин аргумент - список, состоящий из строки таблицы table
Если для данной строки test возвращает t - строка попоадет в разультирующую таблицу,
если test возвращает nil - сторка отфильтровывается
Примеры использования:
Без фильтрации:
 (filter '((1 2 3)(2 3 4)(nil 6 7) (8 9 10)))
Исключить все строки, где есть nil
 (filter '((nil nil 3)(2 3 4)(nil 6 7) (8 9 10)) 
	 #'(lambda(el)(notany #'null el)))
Исключить все строки в первой колонке значение равно 5
 (filter '((1 2 3)(2 3 4)(5 6 7) (8 9 10))
	 #'(lambda(el)(not(= 5 (nth 0 el)))))
"  
  (let ((rez nil))  
    (mapc #'(lambda (el)
	      (if (funcall test el)
		  (setf rez (append rez (list el))))) table) rez))

(defun make-int-list (n-lst)
  "Формирует список, состоящий из целых чисел
Принимает список элементами которого могут быть целые или
списки из двух целых 
Целые непосредственно помещаются в выходной список
Списки из двух преобразуются в список целых в диапазоне от 
первого числа до второго и затем помещаются в результирующий список
Пример использования:
(make-int-list '(5 (6 9) 12 (50 55)))
"
  (apply #'append
	 (mapcar
	  #'(lambda (lst)
	      (let ((rez nil))
		(cond
		  ((listp lst) (make-int-range (first lst) (second lst)))
		  ((numberp lst) (list lst)))))
	  n-lst)))

(defun make-int-range (from to)
  "Возвращает список, состоящий из чисел от from до to включительно
Пример использования:
(make-int-range 30 45)
"
  (let ((rez nil))
    (do 
     ((i from (+ 1 i)))
     ((> i to) rez)
      (setf rez (append rez (list i))))))

(defun make-func-polynom-fit (input x-nom y-nom stepen &optional (out (make-string-output-stream)))
  "Функция для генерации одиночной осредняющей кривой
Пример использования:
input  - \"input.txt\";
x-nom  - от 1 до n;
y-nom  - от 1 до n;
stepen - степень полинома;
(make-func-polynom-fit \"input.txt\" 1 3 6)
"
  (format out "f_~A_~A(x) = ~A~%" x-nom y-nom
	  (let ((str "")
		(ind (format nil "_~A_~A" x-nom y-nom))
		)
	    (dolist (i (make-int-range 0 stepen) (string-right-trim " +" str) )
	      (cond
		((= i 0) (setf str (concatenate 'string str (nth i *const-names*) ind " + " )))
		((= i 1) (setf str (concatenate 'string str (nth i *const-names*) ind "*x + " )))
		(t       (setf str (concatenate 'string str (nth i *const-names*) ind "*x**" (format nil "~A" i) " + " )))))))
  (format out "fit f_~A_~A(x) \"~A\" using ~A:~A via ~A" x-nom y-nom input x-nom y-nom
	  (let ((str "")
		(ind (format nil "_~A_~A" x-nom y-nom)))
	    (dolist (i (make-int-range 0 stepen) (string-right-trim " ," str))
	      (setf str (concatenate 'string str (nth i *const-names*) ind "," )))))
  (get-output-stream-string out))

(defun make-plot (table			;
		  lables		;
		  &key
		    (x1y1 '(1 (2)))
		    (x1y2 nil)
		    (xrange  "[*:*]")
		    (x2range nil) ;;;; "[*:*]"
		    (yrange  "[*:*]")
	            (y2range nil) ;;;; "[*:*]"
		    (mxtics 10)
		    (mytics 10)
		    (tics "out")
		    (tics-scale '(2 1))
		    (terminal "pdfcairo")
		    (terminal-size '(15 15))
		    (terminal-unit "cm")
		    (key "below")
		    (output "gp")
		    (title "GnuPlot Graph")
		    (out (make-string-output-stream))
		    (point-type *point-type*)
		    (line-type -1)
		    (line-width 3)
		    )
  "
table         - прямоугольная таблица со значениями;
lables        - метки со значениями;
x1y1          - перечень графиков для вывода на осях x1 y1 (x (y_1 y_2 ... y_n));
x1y2          - перечень графиков для вывода на осях x1 y2 (x (y_1 y_2 ... y_n));
xrange        - отображаемый диапазон по шкале x;
x2range       - отображаемый диапазон по шкале x2;
yrange        - отображаемый диапазон по шкале y;
y2range       - отображаемый диапазон по шкале y2;
tics          - отображение засечек out|in;
tics-scale    - размер засечек (2 1));
mxtics        - количество подделений по оси x;
mytics        - количество подделений по оси y;
terminal      - тип терминала - pdfcairo;
terminal-size - список, содержащий размеры терминала - (x y); 
terminal-unit - единицы измерения для терминала - cm|in;
key           - место расположения подписей - below|...;
output        - имя файла для помещения результатов работы -  gp;
title         - заголовок для графика - строка;
out           - поток вывода результатов работы функции;
Пример использования:
(make-plot *tbl* *tbl-labes* :x1y1 '(0 (2 3)) :x1y2 '(0 (1 4)))
;;;;(with-output-to-string (out) (format out \"hello, world \") (format out \"~s\" (list 1 2 3))) 
"
  (format out "set terminal ~A size ~A~A,~A~A~%"
	  terminal
	  (first terminal-size )
	  terminal-unit
	  (second terminal-size )
	  terminal-unit)
  (format out "set output \"~A.pdf\"~%" output)
  (format out "~%")
  (if key (format out "set key ~A~%" key))
  (if title (format out "set title \"~A\"~%" title))
  (format out "~%")
  (if xrange  (format out "set xrange  ~A~%"  xrange))
  (if yrange  (format out "set yrange  ~A~%"  yrange))
  (if x2range (format out "set x2range ~A~%"  x2range))
  (if y2range (format out "set y2range ~A~%"  y2range))
  (format out "~%")
  (if tics (format out "set tics ~A~%" tics))
  (if tics-scale (format out "set tics scale ~A,~A~%"
			 (first tics-scale) (second tics-scale)))
  (format out "~%")
  (format out "set xtics nomirror~%")
  (format out "set ytics nomirror~%")
  (format out "~%")
  (if mxtics (format out "set mxtics ~A~%" mxtics))
  (if mytics (format out "set mytics ~A~%" mytics))
  (format out "~%")
  (if x1y1
      (progn
	(format out "plot\\")
	(format out "~A" (out-plot output x1y1 lables))))
  (cond
    ((and x1y2 (null x1y1))
     (format out "plot\\")
     (format out "~A" (out-plot output x1y2 lables :axis "x1y2")))
    ((and x1y2 x1y1)
     (format out ",\\")
     (format out "~A" (out-plot output x1y2 lables :axis "x1y2"))))
;;;;(make-func-polynom-fit \"input.txt\" 1 3 6)
  (format t "~A"(get-output-stream-string out)))

;;;;(make-plot *tbl* *tbl-labes* :x1y1 '(0 (2 3)) :x1y2 '(0 (1 4)))

(defun out-plot(file-name
		lst
		lables
		&key
		  (out (make-string-output-stream))
		  (axis nil)
		  (point-type *point-type*)
		  (line-type -1)
		  (line-width 3)
		  )
  (let ((pt 0))
    (mapc
     #'(lambda(el)
	 (format out "~%\"~A\" " file-name)
	 (format out "using ~A:~A"  (+ (first lst) 1) (+ el 1))
	 (if axis (format out " axis ~A" axis))
	 (if point-type (format out " pt ~A" (nth pt point-type)))
	 (if line-type (format out " lt ~A" line-type)) 
	 (format out "  title \"~A\"" (gethash el *tbl-labes-hash*))
	 (format out ", ")
	 (format out "f_~A_~A(x)"  (+ (first lst) 1) (+ el 1))
	 (if axis (format out " axis ~A" axis))
 	 (if line-type (format out " lt ~A" line-type))
	 (if line-width (format out " lw ~A" line-width))
	 (format out " title \"\"" )
	 (format out ",\\")
	 (if (>= (incf pt) (length point-type)) (setf pt 0)))
     (second lst)))
;;;;
  (string-right-trim "\\," (get-output-stream-string out))
  )

(out-plot "1_CO_NOX-t04.txt" '(0 (2 3 4 5 6 7 8 9)) *tbl-labes*  :axis "x1y1" :point-type *point-type-close*)

plot   '1_CO_NOX-t04.txt' using 1:2 axis x1y2 pt 20 lt -1 title "CO", f1(x) axis x1y2 lt -1 lw 3 dt 1 title "",\

plot
"1_CO_NOX-t04.txt"using 1:3 axis x1y1 pt 5 lt -1  title "y_2", f_1_3(x) axis x1y1 lt -1 lw 3 title "",\
"1_CO_NOX-t04.txt"using 1:4 axis x1y1 pt 7 lt -1  title "y_3", f_1_4(x) axis x1y1 lt -1 lw 3 title "",\
"1_CO_NOX-t04.txt"using 1:5 axis x1y1 pt 9 lt -1  title "y_4", f_1_5(x) axis x1y1 lt -1 lw 3 title "",\
"1_CO_NOX-t04.txt"using 1:6 axis x1y1 pt 11 lt -1  title "NIL", f_1_6(x) axis x1y1 lt -1 lw 3 title "",\
"1_CO_NOX-t04.txt"using 1:7 axis x1y1 pt 13 lt -1  title "NIL", f_1_7(x) axis x1y1 lt -1 lw 3 title "",\
"1_CO_NOX-t04.txt"using 1:8 axis x1y1 pt 15 lt -1  title "NIL", f_1_8(x) axis x1y1 lt -1 lw 3 title "",\
"1_CO_NOX-t04.txt"using 1:9 axis x1y1 pt 5 lt -1  title "NIL", f_1_9(x) axis x1y1 lt -1 lw 3 title "",\
"1_CO_NOX-t04.txt"using 1:10 axis x1y1 pt 7 lt -1  title "NIL", f_1_10(x) axis x1y1 lt -1 lw 3 title ""

