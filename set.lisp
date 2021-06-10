;;;; set.lisp

(in-package #:gnuplot)

(defun symbol-string (sym &key (print-case :downcase))
  (mnas-string:replace-all
   (mnas-string:replace-all 
    (ecase print-case
      (:downcase   (string-downcase   (symbol-name sym)))
      (:upcase     (string-upcase     (symbol-name sym)))
      (:capitalize (string-capitalize (symbol-name sym))))
    "-" "")
   "_" " "))

(defun symbols->stream (stream &rest rest)
  (map nil (lambda (el)
	     (when el (format stream " ~A" (symbol-string el))))
       rest))

(defun symbols->stream-capitalize (stream &rest rest)
  (map nil (lambda (el)
	     (when el (format stream " ~A" (symbol-string el :print-case :capitalize))))
       rest))

(export 'set-grid )
(defun set-grid (&key
		   (stream t)
		   xtics  ytics  ztics
                   x2tics y2tics rtics
		   cbtics
                   polar angle
                   layer
                   line-properties-major line-properties-minor)
"@b(Описание:) set-grid

 @b(Пример использования:)
@begin[lang=lisp](code)
 (set-grid :xtics :xtics :ztics :ztics :layer :back)
@end(code)

 @b(GNUPLOT Syntax:)
@begin[lang=gnuplot](code)
       set grid {{no}{m}xtics} {{no}{m}ytics} {{no}{m}ztics}
                {{no}{m}x2tics} {{no}{m}y2tics} {{no}{m}rtics}
                {{no}{m}cbtics}
                {polar {<angle>}}
                {layerdefault | front | back}
                {{no}vertical}
                {<line-properties-major> {, <line-properties-minor>}}
       unset grid
       show grid
@end(code)
"
  (assert (member xtics '(nil :xtics :m-xtics :no-xtics :no-m-xtics )))
  (assert (member ytics '(nil :ytics :m-ytics :no-ytics :no-m-ytics )))
  (assert (member ztics '(nil :ztics :m-ztics :no-ztics :no-m-ztics )))
  
  (assert (member x2tics '(nil :x2tics :m-x2tics :no-x2tics :no-m-x2tics )))
  (assert (member y2tics '(nil :y2tics :m-y2tics :no-y2tics :no-m-y2tics )))
  (assert (member rtics  '(nil :rtics :m-rtics :no-rtics :no-m-rtics )))

  (assert (member cbtics '(nil :cbtics :m-cbtics :no-cbtics :no-m-cbtics )))

  (assert (member layer '(nil :layer-default :front :back )))

  (format stream "set grid")
  (symbols->stream stream xtics ytics ztics x2tics y2tics rtics cbtics layer)

  (when (and polar (null angle))  (format stream " polar"))
  (when angle                     (format stream " polar ~A" angle))
  
  (when (and line-properties-major (null line-properties-minor))
    (format stream " ~A" line-properties-major))
  
  (when (and line-properties-major line-properties-minor)
    (format stream " ~A, ~A" line-properties-major line-properties-minor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'set-polar )
(defun set-polar (&key (stream t))
"@b(Описание:) set-polar устанавливает отрисовку в полярной системе координат.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (set-polar) => set polar
@end(code)

 @b (GNUPLOT Syntax:)
@begin[lang=gnuplot](code)
 Syntax:
       set polar
       unset polar
       show polar
@end(code)

 @b(Пример использования:)
@begin[lang=gnuplot](code)
 Example:
       set polar
       plot t*sin(t)
       set trange [-2*pi:2*pi]
       set rrange [0:3]
       plot t*sin(t)
@end(code)
"
 (format stream "set polar"))
(defun unset-polar (&key (stream t)) (format stream "unset polar"))

(defun show-polar (&key (stream t)) (format stream "show polar"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'set-theta )
(defun set-theta (&key (stream t) direction clock)
" Polar coordinate plots are by default oriented such that theta = 0 is on the
 right side of the plot, with theta increasing as you proceed counterclockwise
 so that theta = 90 degrees is at the top.  `set theta` allows you to change
 the origin and direction of the polar angular coordinate theta.
      set theta {right|top|left|bottom}
      set theta {clockwise|cw|counterclockwise|ccw}
 `unset theta` restores the default state `set theta right ccw`."
  (assert (member direction '(nil :right :top :left :bottom ) :test #'eq))
  (assert (member clock     '(nil :clockwise :cw :counterclockwise :ccw) :test #'eq))
  (format stream "set theta")
  (symbols->stream stream direction clock))

(set-theta :direction :right :clock :counterclockwise)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'set-*range )
(defun set-*range (axis &key (stream t) min max reverse writeback extend restore)
"@b(Описание:) set-*range 

 @b(GNUPLOT Syntax:)
@begin[lang=gnuplot](code)
       set xrange [{{<min>}:{<max>}}] {{no}reverse} {{no}writeback} {{no}extend}
                  | restore
       show xrange
@end(code)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (set-*range :x  :restore :restore) => set xrange restore
 (set-*range :x :min 1 :max 10)     => set xrange [1:10]
@end(code)
"
  (assert (member axis      '(:x :y :z :x2 :y2 :cb :r :t :u :v)))
  (assert (member reverse   '(nil :no-reverse :reverse)))
  (assert (member writeback '(nil :no-writeback :writeback)))
  (assert (member extend    '(nil :no-extend :extend)))
  (assert (member restore   '(nil :restore)))
  (assert (notevery
	   (complement #'null)
	   (list (or min max reverse writeback extend) restore )))
  
  (format stream "set ~Arange" (symbol-string axis))
  (when (and (null min) (null max) (not restore)) (format stream " []" ))
  (when (and (null min)       max ) (format stream " [:~A]"   max))
  (when (and       min  (null max)) (format stream " [~A:]"   min ))
  (when (and       min        max)  (format stream " [~A:~A]" min max))
  (symbols->stream stream reverse writeback extend restore))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun valid-offset (offset)
"@b(Описание:) valid-offset проверяет правильность фомата смещения подписи к засечкам.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (valid-offset '(2 1)) => T
 (valid-offset '(2 1 5)) => T
 (valid-offset '((:first . 2) (:second . 1) (:graph . 5)))  => T
 (valid-offset '((:screen . 2) (:character . 1))) => T
 (valid-offset '(1)) => NIL
 (valid-offset '(1 2 3 4)) =>NIL
@end(code)

 @b(gnuplot:)
 Смещение <offset> может быть задано в виде x,y или x,y,z.
 Каждая из координат может предваряться словами `first`, `second`, 
 `graph`, `screen` или `character` для указания системы координат.
 <offset> является смещением текста засечки от его расположения по-умолчанию.
 Системой координат поумолчанию является `character`.
 `nooffset` switches off the offset.

 @b(Пример использования:) 

 Перемещает xtics более плотно к графику. 
@begin[lang=gnuplot](code)
       set xtics offset 0,graph 0.05
@end(code)
"
  (labels ((cons-with-key-number (key-lst)
	     (lambda (var)
	       (or (numberp var)
		   (and
		    (consp var)
		    (member (car var) key-lst)
		    (numberp (cdr var)))))))
    (and (every (cons-with-key-number '(:first :second :graph :screen :character)) offset)
	 (<= 2 (length offset) 3))))

(defun valid-position (position)
"
@begin[lang=text](code)
 This section describes placement of the primary, auto-generated key.
 To construct a secondary key or place plot titles elsewhere, see
 `multiple keys`.

 To understand positioning, the best concept is to think of a region, i.e.,
 inside/outside, or one of the margins.  Along with the region, keywords
 `left/center/right` (l/c/r) and `top/center/bottom` (t/c/b) control where
 within the particular region the key should be placed.

 When in `inside` mode, the keywords `left` (l), `right` (r), `top` (t),
 `bottom` (b), and `center` (c) push the key out toward the plot boundary as
 illustrated:

      t/l   t/c   t/r

      c/l    c    c/r

      b/l   b/c   b/r

 When in `outside` mode, automatic placement is similar to the above
 illustration, but with respect to the view, rather than the graph boundary.
 That is, a border is moved inward to make room for the key outside of
 the plotting area, although this may interfere with other labels and may
 cause an error on some devices.  The particular plot border that is moved
 depends upon the position described above and the stacking direction.  For
 options centered in one of the dimensions, there is no ambiguity about which
 border to move.  For the corners, when the stack direction is `vertical`, the
 left or right border is moved inward appropriately.  When the stack direction
 is `horizontal`, the top or bottom border is moved inward appropriately.

 The margin syntax allows automatic placement of key regardless of stack
 direction.  When one of the margins `lmargin` (lm), `rmargin` (rm),
 `tmargin` (tm), and `bmargin` (bm) is combined with a single, non-conflicting
 direction keyword, the following illustrated positions may contain the key:

           l/tm  c/tm  r/tm

      t/lm                  t/rm

      c/lm                  c/rm

      b/lm                  b/rm

           l/bm  c/bm  r/bm

 Keywords `above` and `over` are synonymous with `tmargin`.  For version
 compatibility, `above` or `over` without an additional l/c/r or stack direction
 keyword uses `center` and `horizontal`.  Keywords `below` and `under` are
 synonymous with `bmargin`.  For compatibility, `below` or `under` without an
 additional l/c/r or stack  direction keyword uses `center` and `horizontal`.  A
 further compatibility issue is that `outside` appearing without an additional
 t/b/c or stack direction keyword uses `top`, `right` and `vertical` (i.e., the
 same as t/rm above).

 The <position> can be a simple x,y,z as in previous versions, but these can
 be preceded by one of five keywords (`first`, `second`, `graph`, `screen`,
 `character`) which selects the coordinate system in which the position of
 the first sample line is specified.  See `coordinates` for more details.
 The effect of `left`, `right`, `top`, `bottom`, and `center` when <position>
 is given is to align the key as though it were text positioned using the
 label command, i.e., `left` means left align with key to the right of
 <position>, etc.
@end(code)
"
  (labels ((cons-with-key-number (key-lst)
	     (lambda (var)
	       (or (numberp var)
		   (and
		    (consp var)
		    (member (car var) key-lst)
		    (numberp (cdr var)))))))
    (and (every (cons-with-key-number '(:first :second :graph :screen :character)) position)
	 (<= 2 (length position) 3))))

(defun valid-step-freq (step-freq)
"@b(Описание:) valid-step-freq

 @b(Пример использования:)
@begin[lang=lisp](code)
 (valid-step-freq :autofreq) => nil
 (valid-step-freq '(1 . 2))  =>  [Condition of type TYPE-ERROR]
 (valid-step-freq 10)        => T
 (valid-step-freq '(10.0 20.0 110.0))  => T
 (valid-step-freq '(10.0 20.0 )) => T
@end(code)
"
  (cond
    ((numberp step-freq))        ;;;; <incr>
    ((and (consp step-freq)      ;;;; <start>, <incr> {,<end>}
	  (every #'numberp step-freq)
	  (<= 2 (length step-freq) 3)))))

(defun valid-labels (label)
"@b(Описание:) valid-labels

 @b(Пример использования:)
@begin[lang=lisp](code)
  (valid-labels '((\"Pi\"  3.141592653589793d0) (\"e\"  2.718281828459045))) => T
@end(code)
"
  (cond
    ((and (consp label)
	  (every
	   #'(lambda (el) (and (stringp (first el)) (numberp (second el))))
	   label)))))

(defun valid-colorspec (colorspec)
"@b(Описание:) valid-colorspec проверка на корректность спецификации цвета.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (valid-colorspec \"red\")           => T
 (valid-colorspec \"reda\")          => NIL
 (valid-colorspec '(30 30 20))       => T
 (valid-colorspec '(45 30 30 20))    => T
 (valid-colorspec '( 30 20))         => NIL
 (valid-colorspec '(45 65 20 30 20)) => NI
@end(code)
"
  (cond
    ((and (stringp  colorspec) (gethash colorspec *color-names*) T))
    ((and (integerp colorspec) (<= 0 #xffFFffFF)) T)
    ((and (consp colorspec) (<= 3 (length colorspec) 4)
	  (every
	   #'(lambda (el)
	       (and (integerp el) (<= 0 el #xff)))
	   colorspec)))))

(defun valid-font (font)
"@b(Описание:) valid-font проверка на корректность шрифта.
"
  (or (stringp font)
      (and (consp font)
	   (stringp (first font))
	   (numberp (second font)))))

(defun print-font (font &optional (stream t))
"@b(Описание:) print-font
"
  (when (stringp font) (format stream " font ~S" font))
  (when (and (consp font)
	     (stringp (first font))
	     (numberp (second font)))
    (format stream " font \"~A,~A\"" (first font) (second font))))

(defun print-textcolor (textcolor &optional (stream t))
"@b(Описание:) print-textcolor
"
  (when (stringp textcolor) (format stream " textcolor ~S" textcolor))
  (when (integerp textcolor) (format stream " textcolor \"0x~2,'0X\"" textcolor))
  (when (consp textcolor) (format stream " textcolor \"0x~{~2,'0X~}\"" textcolor)))

(export 'set-*tics )
(defun set-*tics (axis
		  &key
		    (stream t) axis-border mirror in-out scale rotate
		    offset
		    justify add
		    step-freq
		    labels
		    format font
		    enhanced
                    numeric-timedate-geographic
		    rangelimited
		    textcolor
		    )
"@b(Описание:) set-*tics выполняет формирование засечек и их подписей.
 
 @b(Пример использования:)
@begin[lang=lisp](code)
 (set-*tics :y2 :enhanced :no-enhanced :numeric-timedate-geographic 
            :timedate :rangelimited :rangelimited ) => set y2tics noenhanced timedate rangelimited
 (set-*tics :x :justify :right)             => set xtics right
 (set-*tics :x :offset '(1 (:first . 2) 3)) => set xtics offset 1, first 2, 3
 (set-*tics :x :step-freq '(5 2 ))          => set xtics 5, 2
 (set-*tics :x :add :add :step-freq '(5 2 ) :labels '((\"q\" 1.333) (\"s\" 1.66))) 
            => set xtics add 5, 2 (\"q\" 1.333, \"s\" 1.66)
 (set-*tics :x :rotate -15.0  )
 (set-*tics :x :scale :default)   => set xtics scale default
 (set-*tics :x :scale 1.5)        => set xtics scale 1.5
 (set-*tics :x :scale '(1.5 1.0)) => set xtics scale 1.5, 1.0
 (set-*tics :x :format \"%.2f\")  => set xtics format \"%.2f\"
 (set-*tics :x :font '(\"Times\" 10) ) => set xtics font \"Times,10\" 
 (set-*tics :x :in-out :out :font '(\"Times New Roman\" 15) :textcolor '(0 150 145)) =>
set xtics out font \"Times New Roman,15\" textcolor \"0x009691\"
@end(code)

@b(GNUPLOT Syntax:) 
@begin[lang=gnuplot](code)
       set xtics {axis | border} 
                 {{no}mirror}
                 {in | out}
                 {scale {default | <major> {,<minor>}}}
                 {{no}rotate {by <ang>}} 
                 {offset <offset> | nooffset}
                 {left | right | center | autojustify}
                 {add}
                 {  autofreq
                  | <incr>
                  | <start>, <incr> {,<end>}
                  | ({\"<label>\"} <pos> {<level>} {,{\"<label>\"}...) }
                 {format \"formatstring\"}
                 {font \"name{,<size>}\"}
                 {{no}enhanced}
                 { numeric | timedate | geographic }
                 { rangelimited }
                 { textcolor <colorspec> }
       unset xtics
       show xtics
@end(code)
"
  (assert (member axis '(:x :y :z :x2 :y2 :cb)))
  (assert (member axis-border '(nil :axis :border)))
  (assert (member mirror      '(nil :no-mirror :mirror)))
  (assert (member in-out      '(nil :in :out)))
  (assert (or (member offset  '(nil :no-offset )) (valid-offset offset )))
  (assert (member justify     '(nil :left :right :center :autojustify)))
  (assert (member add         '(nil :add)))
  (assert (or (member step-freq   '(nil :auto-freq)) (valid-step-freq step-freq)))
;;;;
  (assert (member enhanced    '(nil :no-enhanced :enhanced )))
  (assert (member numeric-timedate-geographic '(nil :numeric :timedate :geographic )))
  (assert (member rangelimited '(nil :rangelimited )))
  (assert (or (null labels) (valid-labels labels)))
  (assert (or (null format) (stringp format)))
  (assert (or (null font) (stringp font)
	      (and (consp font)
		   (stringp (first font))
		   (numberp (second font)))))
  (assert (or (member scale '(nil :scale :default ))
	      (numberp scale)
	      (and (consp scale)
		   (numberp (first scale))
		   (numberp (second scale)))))
  (assert (or (member rotate '(nil :no-rotate :rotate))
              (numberp rotate)))
  (assert (or (member textcolor '(nil)) (valid-colorspec textcolor)))
  (format stream "set ~Atics" (symbol-string axis))
  (symbols->stream stream axis-border mirror in-out)
  (block scale
    (when (member scale '(:default)) (format stream " scale ~A" (symbol-string scale)))
    (when (numberp scale)            (format stream " scale ~A" scale))
    (when (and (consp scale)
	       (numberp (first scale))
	       (numberp (second scale)))
      (format stream " scale ~A, ~A" (first scale) (second scale))))
  (block rotate
    (when (member rotate '(:no-rotate :rotate)) (symbols->stream stream rotate))
    (when (numberp rotate) (format stream  " rotate by ~A" rotate)))
  (block offset
    (when (eq offset :no-offset) (symbols->stream stream offset))
    (when (valid-offset offset)
      (format stream " offset ~{~A~^, ~}"
	      (mapcar #'(lambda (el)
			  (cond
			    ((numberp el) (format nil "~A" el))
			    ((consp   el) (format nil "~A ~A" (symbol-string (car el)) (cdr el)))))
		      offset))))
  (symbols->stream stream justify add)
  (block step-freq
    (when (eq step-freq :autofreq) (symbols->stream stream step-freq))
    (when (numberp step-freq)      (format stream " ~A" step-freq))
    (when (and (consp step-freq)      
	       (every #'numberp step-freq)
	       (<= 2 (length step-freq) 3))
      (format stream " ~{~A~^, ~}" step-freq))
    (when labels         (format stream " (~{~{~S ~A~}~^, ~})" labels)))
  (when (stringp format) (format stream " format ~S" format))
  (print-font font stream)
  (symbols->stream stream enhanced numeric-timedate-geographic rangelimited)
  (print-textcolor textcolor stream))

(export 'set-m*tics )
(defun set-m*tics (axis &key (stream t) freq-default)
"@b(Описание:) set-m*tics устанавливает количество делений для минорных засечек.

 @b(Переменые:)
@begin(list)
 @item( axis - ось, которой Это применяется :x :y: :z :x2 :y2 :r :t :cd;)
 @item( freq-default - количество делений между основнями засекками или :default.)
@end(list)

 @b(Gnuplot Syntax:)
@begin[lang=lisp](code)
       set mxtics {<freq> | default}
       unset mxtics
       show mxtics
@end(code)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (set-m*tics :y :freq-default 5) => set mytics 5
@end(code)
"
  (assert (member axis '(:x :y :z :x2 :y2 :r :t :cd)))
  (assert (or (member freq-default  '(nil :default))
	      (integerp freq-default)))
  
  (format stream " set m~Atics" (symbol-string axis))
  (when (member freq-default  '(:default)) (symbols->stream stream freq-default))
  (when (integerp freq-default) (format stream " ~D" freq-default)))

(export 'set-key )
(defun set-key ( &key
		   (stream t) on-off default position
		   hor-justification ver-justification
                   vertical-horizontal |Left-Right|
		   enhanced opaque reverse invert
		   samplen spacing width height
		   autotitle-columnheader
		   title title-text title-enhanced title-justification

		   font textcolor
		   box box-line-style box-line-type box-line-width

		   maxcols maxrows
		   )
"@b(Описание:) set-key

 @b(Пример использования:)
@begin[lang=lisp](code)
 (set-key :box :box :box-line-style 5 :box-line-width 2 )
 (set-key :position '(0 5) :box :box :box-line-width 2 :hor-justification :center :ver-justification :center)
 (set-key :position '((:character . 1) (:character . 5)) :box :box :box-line-width 2
	 :hor-justification :left :ver-justification :center )
 (set-key :position '((:screen . 0.5) (:screen . 0.5)) :box :box :box-line-width 2
	 :hor-justification :left :ver-justification :center)
 (set-key :autotitle-columnheader :autotitle_columnheader) => set key autotitle columnheader
@end(code)

@b(GNUPLOT Syntax:)
@begin[lang=gnuplot](code)
       set key {on|off} {default}
             {{inside | outside | fixed} | {lmargin | rmargin | tmargin | bmargin}
               | {at <position>}}
             {left | right | center} {top | bottom | center}
             {vertical | horizontal} {Left | Right}
             {{no}enhanced}
             {{no}opaque}
             {{no}reverse} {{no}invert}
             {samplen <sample_length>} {spacing <line_spacing>}
             {width <width_increment>} {height <height_increment>}
             {{no}autotitle {columnheader}}
             {title {\"<text>\"} {{no}enhanced} {center | left | right}}
             {font \"<face>,<size>\"} {textcolor <colorspec>}
             {{no}box {linestyle <style> | linetype <type> | linewidth <width>}}
             {maxcols {<max no. of columns> | auto}}
             {maxrows {<max no. of rows> | auto}}
       unset key
       show key
@end(code)
"
  (assert (member on-off    '(nil :on :off)))
  (assert (member default   '(nil :default)))
  (assert (or (member position  '(nil :inside :outside :fixed :l-margin :r-margin :t-margin :b-margin))
	      (valid-position position)))
  (assert (member hor-justification '(nil :left :right :center)))
  (assert (member ver-justification '(nil :top :bottom :center)))
  (assert (member vertical-horizontal '(nil :vertical :horizontal)))
  (assert (member |Left-Right| '(nil '|Left| '|Right|)))

  (assert (member enhanced '(nil :enhanced :no-enhanced)))
  (assert (member opaque   '(nil :opaque :no-opaque)))
  (assert (member reverse  '(nil :reverse :no-reverse)))
  (assert (member invert   '(nil :invert :no-invert)))

  (assert (or (member samplen   '(nil)) (numberp samplen)))
  (assert (or (member spacing   '(nil)) (numberp spacing)))
  (assert (or (member width     '(nil)) (numberp width)))
  (assert (or (member height    '(nil)) (numberp height)))
  (assert (or (member autotitle-columnheader
		      '(nil :no-autotitle :autotitle :autotitle_columnheader))))
  (assert (member  title '(nil :title )))
  (assert (or (null title-text) (stringp title-text)))
  (assert (member  title-enhanced '(nil :no-enhanced :enhanced)))
  (assert (member  title-justification '(nil :center :left :right)))
  (assert (eq (null title) (null (or title-text title-enhanced title-justification))))
  
  (assert (or (null font)      (valid-font font)))
  (assert (or (null textcolor) (valid-colorspec textcolor)))

  (assert (member box '(nil :no-box :box)))
  (assert (or (null box-line-style) (numberp box-line-style)))
  (assert (or (null box-line-type)  (numberp box-line-type)))
  (assert (or (null box-line-width) (numberp box-line-width)))
  
  (assert (or
	   (and (null box) (null (or box-line-style box-line-type box-line-width)))
	   (and (eq box :no-box) (null (or box-line-style box-line-type box-line-width)))
	   (and (eq box :box)    (or box-line-style box-line-type box-line-width))))

  (assert (or (member maxcols '(nil :auto)) (integerp maxcols)))
  (assert (or (member maxrows '(nil :auto)) (integerp maxrows)))
  
  (format stream "set key")
  (symbols->stream stream on-off default)
  (block position
    (when (member position
		  '(:inside :outside :fixed :l-margin :r-margin :t-margin :b-margin))
      (symbols->stream stream position))
    
    (when (valid-position position)
      (format stream " at ~{~A~^, ~}"
	      (mapcar #'(lambda (el)
			  (cond
			    ((numberp el) (format nil "~A" el))
			    ((consp   el) (format nil "~A ~A" (symbol-string (car el)) (cdr el)))))
		      position))))
  (symbols->stream stream hor-justification ver-justification vertical-horizontal)
  (symbols->stream-capitalize stream |Left-Right|)
  
  (symbols->stream stream enhanced opaque reverse invert)
  (symbols->stream stream samplen spacing width height autotitle-columnheader) 
  (block title
    (symbols->stream stream title)
    (when (stringp title-text) (format stream " ~S" title-text))
    (symbols->stream stream title-enhanced title-justification))
  (print-font font)
  (print-textcolor textcolor)
  (block box
    (symbols->stream stream box)
    (when box-line-style (format stream " linestyle ~A" box-line-style))
    (when box-line-type  (format stream " linetype ~A" box-line-type))
    (when box-line-width (format stream " linewidth ~A" box-line-width)))
  
  (block maxcols
    (when (eq maxcols :auto) (format stream " maxcols ~A" (symbol-string maxcols)))
    (when (integerp maxcols) (format stream " maxcols ~D" maxcols)))

  (block maxrows
    (when (eq maxrows :auto) (format stream " maxrows ~A" (symbol-string maxrows)))
    (when (integerp maxrows) (format stream " maxrows ~D" maxrows))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'set-title )
(defun set-title (text offset font textcolor enhanced)
"@b(Описание:) set-title

 @b(GNUPLOT Syntax:)
@begin[lang=gnuplot](code)
       set title {\"<title-text>\"} {offset <offset>} {font \"<font>{,<size>}\"}
                 {{textcolor | tc} {<colorspec> | default}} {{no}enhanced}
       show title
@end(code)
"
  (assert text)
  (assert offset)
  (assert font)
  (assert textcolor)
  (assert enhanced)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

"set terminal ~A fontscale ~A size ~A~A,~A~A~%"
"set termoption enhanced~%"
"set output \"~A\"~%"
"set title \"~A\"~%"
"set tics ~A~%"
