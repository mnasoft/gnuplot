;;;; gnuplot.lisp

(in-package #:gnuplot)

;;; "gnuplot" goes here. Hacks and glory await!

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

(defun make-plot (table
		  &key
		    (x1y1 nil)
		    (x1y2 nil)
		    (terminal "pdfcairo")
		    (terminal-size '(15 15))
		    (terminal-unit "cm")
		    (key "below")
		    (output "gp"))
  (format t "set terminal ~A size ~A~A,~A~A~%"
	  terminal
	  (first terminal-size )
	  terminal-unit
	  (second terminal-size )
	  terminal-unit)
  (format t "set output \"~A.pdf\"~%" output)
  (format t "set key ~A~%" key)
  
  )

(defun make-func-polynom-fit (input x-nom y-nom stepen)
  (format t "f_~A_~A(x) = ~A~%" x-nom y-nom
	  (let ((str "")
		(ind (format nil "_~A_~A" x-nom y-nom))
		)
	    (dolist (i (make-int-range 0 stepen) (string-right-trim " +" str) )
	      (cond
		((= i 0) (setf str (concatenate 'string str (nth i *const-names*) ind " + " )))
		((= i 1) (setf str (concatenate 'string str (nth i *const-names*) ind "*x + " )))
		(t       (setf str (concatenate 'string str (nth i *const-names*) ind "*x**" (format nil "~A" i) " + " )))))))
  (format t "fit f_~A_~A(x) \"~A\" using ~A:~A via ~A" x-nom y-nom input x-nom y-nom
	  (let ((str "")
		(ind (format nil "_~A_~A" x-nom y-nom)))
	    (dolist (i (make-int-range 0 stepen) (string-right-trim " ," str))
	      (setf str (concatenate 'string str (nth i *const-names*) ind "," ))))))


(make-func-polynom-fit "1_CO_NOX-t04.txt" 1 3 4)

(defparameter *const-names*(list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "w") )


(make-plot 5)
