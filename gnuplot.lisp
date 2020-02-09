;;;; gnuplot.lisp

(in-package #:gnuplot)

(annot:enable-annot-syntax)

(defparameter *const-names*
  (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "w")
  "Имена переменных для использования при создании осредняющих функций 
при помощи make-func-polynom-fit")

@export
@annot.doc:doc
"Возвращает список, состоящий из чисел от from до to включительно

 @b(Пример использования:)
@begin[lang=lisp](code)
 (make-int-range 30 45) => (30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45)
@end(code)
"
(defun make-int-range (from to)
  (let ((rez nil))
    (do 
     ((i from (+ 1 i)))
     ((> i to) rez)
      (setf rez (append rez (list i))))))

@export
@annot.doc:doc
"@b(Описание:) make-int-list формирует список, состоящий из целых чисел
Принимает список элементами которого могут быть целые или
списки из двух целых 
Целые непосредственно помещаются в выходной список
Списки из двух преобразуются в список целых в диапазоне от 
первого числа до второго и затем помещаются в результирующий список

 @b(Пример использования:)
@begin[lang=lisp](code)
 (make-int-list '(5 (6 9) 12 (50 55)))
@end(code)
"
(defun make-int-list (n-lst)
  (apply #'append
	 (mapcar
	  #'(lambda (lst)
		(cond
		  ((listp lst) (make-int-range (first lst) (second lst)))
		  ((numberp lst) (list lst))))
	  n-lst)))

@export
@annot.doc:doc
"@b(Описание:) extract выбирает из списка списков val-list 
колонки с номерами, находящиеся в списке col-number-list
Возвращает список списков из отобранных колонок.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (extract '(0 (2 4)) 
	 '(( 0  1  2  3  4  5  6)
	   (10 11 12 13 14 15 16)
	   (20 21 22 23 24 25 26)
	   (30 31 32 33 34 35 36))) 
=>
 '((0  2  3  4) 
  (10 12 13 14) 
  (20 22 23 24)
  (30 32 33 34))
@end(code)
"
(defun extract (col-number-list val-list)
  (mapcar   #'(lambda (el)
		(mapcar
		 #'(lambda (n) (nth n el))
		 (make-int-list col-number-list)))
	    val-list))

@export
@annot.doc:doc
"@b(Описание:) make-func-polynom-fit функция для генерации одиночной осредняющей кривой

 @b(Переменые:)
@begin(list)
 @item(input  - \"input.txt\";)
 @item(x-nom  - от 0 до n-1; )  
 @item(y-nom  - от 0 до n-1;) 
 @item(stepen - степень полинома;)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
  (make-func-polynom-fit \"input.txt\" 1 3 6)
@end(code)
"
(defun make-func-polynom-fit (input x-nom y-nom stepen &optional (out (make-string-output-stream)))
  (format out "f_~A_~A(x) = ~A~%" (+ x-nom 1) (+ y-nom 1)
	  (let ((str "")
		(ind (format nil "_~A_~A" (+ x-nom 1) (+ y-nom 1)))
		)
	    (dolist (i (make-int-range 0 stepen) (string-right-trim " +" str) )
	      (cond
		((= i 0) (setf str (concatenate 'string str (nth i *const-names*) ind " + " )))
		((= i 1) (setf str (concatenate 'string str (nth i *const-names*) ind "*x + " )))
		(t       (setf str (concatenate 'string str (nth i *const-names*) ind "*x**" (format nil "~A" i) " + " )))))))
  (format out "fit f_~A_~A(x) \"~A\" using ~A:~A via ~A" (+ x-nom 1) (+ y-nom 1) input (+ x-nom 1) (+ y-nom 1)
	  (let ((str "")
		(ind (format nil "_~A_~A" (+ x-nom 1) (+ y-nom 1))))
	    (dolist (i (make-int-range 0 stepen) (string-right-trim " ," str))
	      (setf str (concatenate 'string str (nth i *const-names*) ind "," )))))
  (format out "~%")
  (get-output-stream-string out))

@export
@annot.doc:doc
"@b(Описание:) out-func-polynom-fit

 @b(Пример использования:)
@begin[lang=lisp](code)
 (out-func-polynom-fit \"1_CO_NOX-t04.txt\" '(0 (2 3)) 6)
@end(code)
"
(defun out-func-polynom-fit (file-name lst stepen)
  (let ( (out (make-string-output-stream)))
    (mapc
     #'(lambda(el)
	 (format out "~A" 
		 (make-func-polynom-fit file-name (car lst) el stepen)))
     (second lst))
    (get-output-stream-string out)))

@export
@annot.doc:doc
"@b(Описание:) out-plot

 @b(Переменые:)
@begin(list)
 @item(data-file         - строка, представляющая имя файла в котором находятся данные; )
 @item(lst               - список, содержащий номера колонок, предназначенные для вывода (x0 (y0 ... yn-1)). Нумерация начинается с нуля; )
 @item(ht-labels         - хеш таблица, содержащая имена переменных )
 @item(point-type-number -  )
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
  (out-plot \"1_CO_NOX-t04.txt\" '(0 (2  4 5)) *tbl-labes-hash*  :axis \"x1y1\" :point-type *point-type-fill* :point-type-number 3)
@end(code)
"
(defun out-plot (data-file lst ht-labels
		&key (axis nil) (point-type *point-type-all*) (point-scale 2) (line-type -1) (line-width 3) (point-type-number 0))
  (let ((pt point-type-number)		  ; номер точки
	(out (make-string-output-stream)) ; поток для вывода
	)
    (mapc
     #'(lambda(el)
	 (format out "~%\"~A\" " data-file)
	 (format out "using ~A:~A"  (+ (first lst) 1) (+ el 1))
	 (if axis (format out " axis ~A" axis))
	 (if point-type (format out " pt ~A" (nth pt point-type)))
         (if point-scale (format out " ps ~A" point-scale))
	 (if line-type (format out " lt ~A" line-type))
	 (if line-width (format out " lw ~A" line-width))
	 (format out "  title \"~A\"" (gethash el ht-labels))
	 (format out ", ")
	 (format out "f_~A_~A(x)"  (+ (first lst) 1) (+ el 1))
	 (if axis (format out " axis ~A" axis))
 	 (if line-type (format out " lt ~A" line-type))
	 (if line-width (format out " lw ~A" line-width))
	 (format out " title \"\"" )
	 (format out ",\\")
	 (if (>= (incf pt) (length point-type)) (setf pt 0)))
     (second lst))
    (values (string-right-trim "\\," (get-output-stream-string out)) pt)))

@export
@annot.doc:doc
" @b(Описание:) format-n-string формирует строку для использования с функцией format
для вывода n значений в формате str-format c разделителями str-delimiter.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (format-n-string 15)
 (format-n-string 15 :str-delimiter \",\")
 (format-n-string 15 :str-format \"~S\")
@end(code)
"
(defun format-n-string (n &key (str-format "~A") (str-delimiter " "))
  (let ((str ""))
    (dotimes (i n (concatenate 'string (string-right-trim str-delimiter str) "~%"))
      (setf str (concatenate 'string str str-format str-delimiter)))))

@export
@annot.doc:doc
"@b(Описание:) out-table выводит 2d-список @b(table) в поток @b(out).
 
 @b(Переменые:)
@begin(list)
 @item(- table 2d-список;)
 @item(- out поток вывода;)
 @item(- формат вывода данных str-format; )
 @item(- разделитель элементов списка str-delimiter.)
@end(list)

@b(Пример использования:)
@begin[lang=lisp](code)
 (out-table '((1 2 3)(4 5 6 ) (7 8) (9))) =>
 1 2 3
 4 5 6
 7 8
 9
 T
@end(code)
"
(defun out-table (table  &key (out t) (str-format "~A") (str-delimiter " "))
  (mapcar
   #'(lambda (el)
       (apply #'format out (format-n-string (length el) :str-format str-format :str-delimiter str-delimiter) el))
   table)
  t)

@export
@annot.doc:doc
"@b(Описание:) make-hash-table-lables создает хеш-таблицу с:
@begin(list)
 @item(ключами являющимися номерами колонок в таблице, содержащей данные для построения графиков (нумерация начинается с 0);)
 @item(значениями - строками, отображаемыми как подписи к графикам. )
@end(list)

@b(Переменые:)
@begin(list)
 @item(lables - список каждым элементом которого является список следующего вида:

 (номер_столбца строка_для_подписи строка_для_размерности) )
@end(list)

@b(Пример использования:)
@begin[lang=lisp](code)
 (make-hash-table-lables
  '((0 \"x_1\" \"s\")
    (1 \"s_1\" \"m\")
    (2 \"v_1\" \"m/s\")
    (3 \"a_1\" \"m/(s^2)\")))
@end(code)
"
(defun make-hash-table-lables (lables)
  (let ((ht (make-hash-table)))
    (mapc #'(lambda (el) (setf (gethash (first el) ht)
			       (second el)))
	  lables)
    ht))

@export
@annot.doc:doc
"@b(Описание:) filter выполняет фильтрацию строк из таблицы по определенному критерию,
задаваемому в функции test.

 Функция test принимает один аргумент - список, состоящий из строки таблицы table:
@begin(list)
 @item(если для данной строки test возвращает t - строка попоадет в разультирующую таблицу;)
 @item(если test возвращает nil - сторка отфильтровывается.)
@end(list)
 
 @b(Пример использования:)
@begin[lang=lisp](code)
;; Без фильтрации:
 (filter '((1 2 3)(2 3 nil)(nil 6 7) (8 9 10))) => ((1 2 3) (2 3 NIL) (NIL 6 7) (8 9 10))

;; Исключить все строки, где есть nil
 (filter '((nil nil 3)(2 3 4)(nil 6 7) (8 9 10)) 
	 #'(lambda(el)(notany #'null el))) => ((2 3 4) (8 9 10))

;; Исключить все строки в первой колонке значение равно 5
 (filter '((1 2 3)(2 3 4)(5 6 7) (8 9 10))
	 #'(lambda(el)(not(= 5 (nth 0 el))))) => ((1 2 3) (2 3 4) (8 9 10))
@end(code)
" 
(defun filter (table &optional (test #'(lambda (el)(if el t t))))
  (let ((rez nil))  
    (mapc #'(lambda (el)
	      (if (funcall test el)
		  (setf rez (append rez (list el)))))
	  table)
    rez))

@export
@annot.doc:doc
"@b(Описание:) make-plot

 @b(Переменые:)
@begin(list)
 @item(table         - прямоугольная таблица со значениями;)
 @item(ht-labels     - хешированная таблица у которой: 
                   - ключ содержит индекс столбца таблицы;
                   - значение содержит подпись столбца таблицы.
                Нумерация начинается с 0; )
 @item(x1y1          - перечень графиков для вывода на осях x1 y1 (x (y_1 y_2 ... y_n)); )
 @item(x1y2          - перечень графиков для вывода на осях x1 y2 (x (y_1 y_2 ... y_n)); )
 @item(xrange        - отображаемый диапазон по шкале x; )
 @item(x2range       - отображаемый диапазон по шкале x2; )
 @item(yrange        - отображаемый диапазон по шкале y; )
 @item(y2range       - отображаемый диапазон по шкале y2; )
 @item(tics          - отображение засечек out|in; )
 @item(tics-scale    - размер засечек (2 1); )
 @item(mxtics        - количество подделений по оси x; )
 @item(mytics        - количество подделений по оси y; )
 @item(terminal      - тип терминала - pdfcairo; )
 @item(terminal-size - список, содержащий размеры терминала - (x y);  )
 @item(terminal-unit - единицы измерения для терминала - cm|in; )
 @item(key           - место расположения подписей - below|...; )
 @item(output        - имя файла для помещения результатов работы -  gp; )
 @item(title         - заголовок для графика - строка. )
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (make-plot gnuplot:*tbl* gnuplot:*tbl-labes-hash* :x1y1 '(0 (2 3)) :x1y2 '(0 (1 4)))
 (with-output-to-string (out) (format out \"hello, world \") (format out \"~s\" (list 1 2 3))) 
@end(code)
"
(defun make-plot (table			;
		  ht-labels		;
		  &key
		    (x1y1 '(1 (2)))
		    (x1y2 nil)
		    (xrange  "[*:*]")
		    (x2range "[*:*]")
		    (yrange  "[*:*]")
	            (y2range "[*:*]")
		    (mxtics 5)
		    (mytics 5)
		    (tics "out")
		    (tics-scale '(2 1))
		    (terminal "pdfcairo")
		    (terminal-size '(30 30))
		    (terminal-unit "cm")
		    (terminal-fontscale 1)
		    (key "below")
		    (output "gp")
		    (output-path "")
		    (title "GnuPlot Graph")
		    (point-type *point-type-all*)
		    (line-type -1)
		    (line-width 3)
		    (stepen 5)
		    (point-number 0)
		    )

  (let ((str-pt (list "" point-number))
	(out (make-string-output-stream)) ; поток вывода результатов работы функции
        (fn-pdf (concatenate 'string output-path output ".pdf"))
        (fn-txt (concatenate 'string output-path output ".txt"))
        (fn-gnuplot (concatenate 'string output-path output ".gnuplot"))
	(f-txt nil)
        (f-gnuplot nil)
	)
    (with-open-file (f-out fn-txt :direction :output :if-exists :overwrite :if-does-not-exist :create)
      (out-table table :out f-out)
      (setf f-txt (uiop:file-pathname-p f-out))
      )
    (format out "set terminal ~A fontscale ~A size ~A~A,~A~A~%"
	    terminal
            terminal-fontscale
	    (first terminal-size )
	    terminal-unit
	    (second terminal-size )
	    terminal-unit)
    (format out "set termoption enhanced~%")
    (format out "set output \"~A\"~%" fn-pdf)
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
;;;;
    (if x1y1 (format out "~A~%" (out-func-polynom-fit fn-txt x1y1 stepen)))
    (if x1y2 (format out "~A~%" (out-func-polynom-fit fn-txt x1y2 stepen)))
    (format out "~%")
    (if x1y1
	(progn
	  (format out "plot\\")
	  (setf str-pt (multiple-value-list
			(out-plot fn-txt x1y1 ht-labels :line-type line-type :line-width line-width :point-type point-type :point-type-number (cadr str-pt))))
	  (format out "~A" (car str-pt))))
    (cond
      ((and x1y2 (null x1y1))
       (format out "plot\\")
       (setf str-pt (multiple-value-list
		     (out-plot fn-txt x1y2 ht-labels :axis "x1y2" :line-type line-type :line-width line-width :point-type point-type :point-type-number (cadr str-pt))))
       (format out "~A" (car str-pt)))
      ((and x1y2 x1y1)
       (format out ",\\")
       (setf str-pt (multiple-value-list
		     (out-plot fn-txt x1y2 ht-labels :axis "x1y2" :line-type line-type :line-width line-width :point-type point-type :point-type-number (cadr str-pt))))
       (format out "~A" (car str-pt))))
    (format out "~%set grid xtics ytics mxtics mytics lt -1 lw 3, lt -1 lw 1")
    (format out "~%~%set output \"~A\"\; replot\; set output \"0.pdf\"" fn-pdf)
    (with-open-file
	(f-out fn-gnuplot :direction :output :if-exists :overwrite :if-does-not-exist :create)
      (format f-out "~A"(get-output-stream-string out))
      (setf f-gnuplot (uiop:file-pathname-p f-out)))
    (values f-gnuplot f-txt))) 
