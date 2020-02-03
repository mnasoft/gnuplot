;;;; set-grid.lisp

(in-package #:gnuplot)

(annot:enable-annot-syntax)

(defun symbol-string (sym)
  (mnas-string:replace-all (string-downcase (symbol-name sym)) "-" ""))

(defun symbols->stream (stream &rest rest)
  (map nil (lambda (el) (when el (format stream " ~A" (symbol-string el)))) rest))

@export
@annot.doc:doc
"@b(Описание:) help-set-grid возврвщает помощь по команде gnuplot
@begin[lang=gnuplot](code)
  set grid
@end(code)
"
(defun help-set-grid (&optional (stream t))
  (format stream 
	  " The `set grid` command allows grid lines to be drawn on the plot.

 Syntax:
       set grid {{no}{m}xtics} {{no}{m}ytics} {{no}{m}ztics}
                {{no}{m}x2tics} {{no}{m}y2tics} {{no}{m}rtics}
                {{no}{m}cbtics}
                {polar {<angle>}}
                {layerdefault | front | back}
                {<line-properties-major> {, <line-properties-minor>}}
       unset grid
       show grid

 The grid can be enabled and disabled for the major and/or minor tic
 marks on any axis, and the linetype and linewidth can be specified
 for major and minor grid lines, also via a predefined linestyle, as
 far as the active terminal driver supports this (see `set style line`).

 A polar grid can be drawn for 2D plots.  This is the default action of 
 `set grid` if the program is already in polar mode, but can be enabled
 explicitly by `set grid polar <angle> rtics` whether or not the program is in
 polar mode.  Circles are drawn to intersect major and/or minor tics along the
 r axis, and radial lines are drawn with a spacing of <angle>.  Tic marks
 around the perimeter are controlled by `set ttics`, but these do not produce
 radial grid lines.

 The pertinent tics must be enabled before `set grid` can draw them; `gnuplot`
 will quietly ignore instructions to draw grid lines at non-existent tics, but
 they will appear if the tics are subsequently enabled.

 If no linetype is specified for the minor gridlines, the same linetype as the
 major gridlines is used.  The default polar angle is 30 degrees.

 If `front` is given, the grid is drawn on top of the graphed data. If
 `back` is given, the grid is drawn underneath the graphed data. Using
 `front` will prevent the grid from being obscured by dense data. The
 default setup, `layerdefault`, is equivalent to `back` for 2D plots.
 In 3D plots the default is to split up the grid and the graph box into
 two layers: one behind, the other in front of the plotted data and
 functions. Since `hidden3d` mode does its own sorting, it ignores
 all grid drawing order options and passes the grid lines through the
 hidden line removal machinery instead. These options actually affect
 not only the grid, but also the lines output by `set border` and the
 various ticmarks (see `set xtics`).

 Z grid lines are drawn on the bottom of the plot.  This looks better if a
 partial box is drawn around the plot---see `set border`."
	  ))


@export
@annot.doc:doc
"@b(Описание:) set-grid
@b(Пример использования:)
@begin[lang=lisp](code)
 (set-grid :xtics :xtics :ztics :ztics :layer :back)
@end(code)
"
(defun set-grid (&key
		   (stream t)
		   xtics  ytics  ztics
                   x2tics y2tics rtics
		   cbtics
                   polar angle
                   layer
                   line-properties-major line-properties-minor)
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

@export
@annot.doc:doc
"@b(Описание:) help-set-polar выводит помощь по команде gnuplot

@begin[lang=gnuplot](code)
 set polar
@end(code)
"
(defun help-set-polar (&optional (stream t))
  (format stream "
 The `set polar` command changes the meaning of the plot from rectangular
 coordinates to polar coordinates.

 Syntax:
       set polar
       unset polar
       show polar

 In polar coordinates, the dummy variable (t) represents an angle theta.
 The default range of t is [0:2*pi], or [0:360] if degree units have been
 selected (see `set angles`).

 The command `unset polar` changes the meaning of the plot back to the default
 rectangular coordinate system.

 The `set polar` command is not supported for `splot`s.  See the `set mapping`
 command for similar functionality for `splot`s.

 While in polar coordinates the meaning of an expression in t is really
 r = f(t), where t is an angle of rotation.  The trange controls the domain
 (the angle) of the function. The r, x and y ranges control the extent of the
 graph in the x and y directions.  Each of these ranges, as well as the
 rrange, may be autoscaled or set explicitly.  For details, see `set rrange`
 and `set xrange`.

 Example:
       set polar
       plot t*sin(t)
       set trange [-2*pi:2*pi]
       set rrange [0:3]
       plot t*sin(t)

 The first `plot` uses the default polar angular domain of 0 to 2*pi.  The
 radius and the size of the graph are scaled automatically.  The second `plot`
 expands the domain, and restricts the size of the graph to the area within
 3 units of the origin.  This has the effect of limiting x and y to [-3:3].

 By default polar plots are oriented such that theta=0 is at the far right,
 with theta increasing counterclockwise.  You can change both the origin and
 the sense explicitly.  See `set theta`.

 You may want to `set size square` to have `gnuplot` try to make the aspect
 ratio equal to unity, so that circles look circular.  Tic marks around the
 perimeter can be specified using `set ttics`.

 See also
 polar demos (polar.dem)
 and
 polar data plot (poldat.dem).
"
	  ))

@export
@annot.doc:doc
"@b(Описание:) set-polar устанавливает отрисовку в полярной системе координат.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (set-polar) => set polar
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
(defun set-polar (&key (stream t)) (format stream "set polar"))

(defun unset-polar (&key (stream t)) (format stream "unset polar"))

(defun show-polar (&key (stream t)) (format stream "show polar"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
@annot.doc:doc
" Polar coordinate plots are by default oriented such that theta = 0 is on the
 right side of the plot, with theta increasing as you proceed counterclockwise
 so that theta = 90 degrees is at the top.  `set theta` allows you to change
 the origin and direction of the polar angular coordinate theta.
      set theta {right|top|left|bottom}
      set theta {clockwise|cw|counterclockwise|ccw}
 `unset theta` restores the default state `set theta right ccw`."
(defun set-theta (&key (stream t) direction clock)
  (assert (member direction '(nil :right :top :left :bottom ) :test #'eq))
  (assert (member clock     '(nil :clockwise :cw :counterclockwise :ccw) :test #'eq))
  (format stream "set theta")
  (symbols->stream stream direction clock))

(set-theta :direction :right :clock :counterclockwise)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
(defun help-set-*range (&optional (stream t))
  (format stream
	  "
 The `set xrange` command sets the horizontal range that will be displayed.
 A similar command exists for each of the other axes, as well as for the
 polar radius r and the parametric variables t, u, and v.

 Syntax:
       set xrange [{{<min>}:{<max>}}] {{no}reverse} {{no}writeback} {{no}extend}
                  | restore
       show xrange

 where <min> and <max> terms are constants, expressions or an asterisk to set
 autoscaling.  If the data are time/date, you must give the range as a quoted
 string according to the `set timefmt` format.
 If <min> or <max> is omitted the current value will not be changed.
 See below for full autoscaling syntax.  See also `noextend`.

 The same syntax applies to `yrange`, `zrange`, `x2range`, `y2range`, `cbrange`,
 `rrange`, `trange`, `urange` and `vrange`.

 See `set link` for options that link the ranges of x and x2, or y and y2.

 The `reverse` option reverses the direction of an autoscaled axis. For example,
 if the data values range from 10 to 100, it will autoscale to the equivalent of
Press return for more: 
 set xrange [100:10].  The `reverse` flag has no effect if the axis is not
 autoscaled. NB: This is a change introduced in version 4.7.

 Autoscaling:  If <min> (the same applies for correspondingly to <max>) is
 an asterisk `*` autoscaling is turned on.  The range in which autoscaling
 is being performed may be limited by a lower bound <lb> or an upper bound
 <ub> or both.  The syntax is 
       { <lb> < } * { < <ub> }
 For example,
       0 < * < 200
 sets <lb> = 0 and <ub> = 200.  With such a setting <min> would be autoscaled,
 but its final value will be between 0 and 200 (both inclusive despite the
 '<' sign).  If no lower or upper bound is specified, the '<' to also be
 omitted.  If <ub> is lower than <lb> the constraints will be turned off
 and full autoscaling will happen.
 This feature is useful to plot measured data with autoscaling but providing
 a limit on the range, to clip outliers, or to guarantee a minimum range
 that will be displayed even if the data would not need such a big range. 

 The `writeback` option essentially saves the range found by `autoscale` in
 the buffers that would be filled by `set xrange`.  This is useful if you wish
 to plot several functions together but have the range determined by only
Press return for more: 
 some of them.  The `writeback` operation is performed during the `plot`
 execution, so it must be specified before that command.  To restore,
 the last saved horizontal range use `set xrange restore`.  For example,

       set xrange [-10:10]
       set yrange [] writeback
       plot sin(x)
       set yrange restore
       replot x/2

 results in a yrange of [-1:1] as found only from the range of sin(x); the
 [-5:5] range of x/2 is ignored.  Executing `show yrange` after each command
 in the above example should help you understand what is going on.

 In 2D, `xrange` and `yrange` determine the extent of the axes, `trange`
 determines the range of the parametric variable in parametric mode or the
 range of the angle in polar mode.  Similarly in parametric 3D, `xrange`,
 `yrange`, and `zrange` govern the axes and `urange` and `vrange` govern the
 parametric variables.

 In polar mode, `rrange` determines the radial range plotted.  <rmin> acts as
 an additive constant to the radius, whereas <rmax> acts as a clip to the
Press return for more: 
 radius---no point with radius greater than <rmax> will be plotted.  `xrange`
 and `yrange` are affected---the ranges can be set as if the graph was of
 r(t)-rmin, with rmin added to all the labels.

 Any range may be partially or totally autoscaled, although it may not make
 sense to autoscale a parametric variable unless it is plotted with data.

 Ranges may also be specified on the `plot` command line.  A range given on
 the plot line will be used for that single `plot` command; a range given by
 a `set` command will be used for all subsequent plots that do not specify
 their own ranges.  The same holds true for `splot`.

 Examples:

 To set the xrange to the default:
       set xrange [-10:10]

 To set the yrange to increase downwards:
       set yrange [10:-10]

 To change zmax to 10 without affecting zmin (which may still be autoscaled):
       set zrange [:10]
Press return for more: 

 To autoscale xmin while leaving xmax unchanged:
       set xrange [*:]

 To autoscale xmin but keeping xmin positive:
       set xrange [0<*:]

 To autoscale x but keep minimum range of 10 to 50 (actual might be larger):
       set xrange [*<10:50<*]

 Autoscaling but limit maximum xrange to -1000 to 1000, i.e. autoscaling
 within [-1000:1000]
       set xrange [-1000<*:*<1000]

 Make sure xmin is somewhere between -200 and 100:
       set xrange [-200<*<100:]

(set-*range \"y\" :no-reverse t :writeback t)
"
	  ))

@export
@annot.doc:doc
"@b(Описание:)
@begin[lang=gnuplot](code)
 Syntax:
       set xrange [{{<min>}:{<max>}}] {{no}reverse} {{no}writeback} {{no}extend}
                  | restore
       show xrange
@end(code)

@b(Пример использования:)
 @begin[lang=lisp](code)
 (set-*range :x  :restore :restore)
 (set-*range :x :min 1 :max 10)
 @end(code)
"
(defun set-*range (axis &key (stream t) min max reverse writeback extend restore)
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

(defun help-set-*tics (&optional (stream t))
  "Помощь по командам gnuplot:
set xtics, set ytics, set ztics, set x2tics, set y2tics 
  "
  (format stream "
 help xtics
 Fine control of the major (labeled) tics on the x axis is possible with the
 `set xtics` command.  The tics may be turned off with the `unset xtics`
 command, and may be turned on (the default state) with `set xtics`.  Similar
 commands control the major tics on the y, z, x2 and y2 axes.

 Syntax:
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

 The same syntax applies to `ytics`, `ztics`, `x2tics`, `y2tics` and `cbtics`.

 `axis` or `border` tells `gnuplot` to put the tics (both the tics themselves
 and the accompanying labels) along the axis or the border, respectively.  If
 the axis is very close to the border, the `axis` option will move the
 tic labels to outside the border.  The relevant margin settings will usually
 be sized badly by the automatic layout algorithm in this case.

 `mirror` tells `gnuplot` to put unlabeled tics at the same positions on the
 opposite border.  `nomirror` does what you think it does.

 `in` and `out` change the tic marks to be drawn inwards or outwards.

 With `scale`, the size of the tic marks can be adjusted. If <minor> is not
 specified, it is 0.5*<major>.  The default size 1.0 for major tics and 0.5
 for minor tics is requested by `scale default`.

 `rotate` asks `gnuplot` to rotate the text through 90 degrees, which will be
 done if the terminal driver in use supports text rotation.  `norotate`
 cancels this. `rotate by <ang>` asks for rotation by <ang> degrees, supported
 by some terminal types.


 The defaults are `border mirror norotate` for tics on the x and y axes, and
 `border nomirror norotate` for tics on the x2 and y2 axes.  For the z axis,
 the `{axis | border}` option is not available and the default is
 `nomirror`.  If you do want to mirror the z-axis tics, you might want to
 create a bit more room for them with `set border`.

 The <offset> is specified by either x,y or x,y,z, and may be preceded by
 `first`, `second`, `graph`, `screen`, or `character` to select the
 coordinate system. <offset> is the offset of the tics texts from their
 default positions, while the default coordinate system is `character`.
 See `coordinates` for details. `nooffset` switches off the offset.

 Example:

 Move xtics more closely to the plot.
       set xtics offset 0,graph 0.05

 By default, tic labels are justified automatically depending on the axis and
 rotation angle to produce aesthetically pleasing results. If this is not
 desired, justification can be overridden with an explicit `left`, `right` or
 `center` keyword. `autojustify` restores the default behavior.


 `set xtics` with no options restores the default border or axis if xtics are
 being displayed;  otherwise it has no effect.  Any previously specified tic
 frequency or position {and labels} are retained.

 Positions of the tics are calculated automatically by default or if the
 `autofreq` option is given; otherwise they may be specified in either of
 two forms:

 The implicit <start>, <incr>, <end> form specifies that a series of tics will
 be plotted on the axis between the values <start> and <end> with an increment
 of <incr>.  If <end> is not given, it is assumed to be infinity.  The
 increment may be negative.  If neither <start> nor <end> is given, <start> is
 assumed to be negative infinity, <end> is assumed to be positive infinity,
 and the tics will be drawn at integral multiples of <incr>.  If the axis is
 logarithmic, the increment will be used as a multiplicative factor.

 If you specify to a negative <start> or <incr> after a numerical value
 (e.g., `rotate by <angle>` or `offset <offset>`), the parser fails because
 it subtracts <start> or <incr> from that value.  As a workaround, specify
 `0-<start>` resp. `0-<incr>` in that case.

 Example:

       set xtics border offset 0,0.5 -5,1,5
 Fails with 'invalid expression' at the last comma.
       set xtics border offset 0,0.5 0-5,1,5
 or
       set xtics offset 0,0.5 border -5,1,5
 Sets tics at the border, tics text with an offset of 0,0.5 characters, and
 sets the start, increment, and end to -5, 1, and 5, as requested.

 The `set grid` options 'front', 'back' and 'layerdefault' affect the drawing
 order of the xtics, too.

 Examples:

 Make tics at 0, 0.5, 1, 1.5, ..., 9.5, 10.
       set xtics 0,.5,10

 Make tics at ..., -10, -5, 0, 5, 10, ...
       set xtics 5

 Make tics at 1, 100, 1e4, 1e6, 1e8.
       set logscale x; set xtics 1,100,1e8


 The explicit (\"<label>\" <pos> <level>, ...) form allows arbitrary tic
 positions or non-numeric tic labels.  In this form, the tics do not
 need to be listed in numerical order.  Each tic has a
 position, optionally with a label.  Note that the label is
 a string enclosed by quotes.  It may be a constant string, such as
 \"hello\", may contain formatting information for converting the
 position into its label, such as \"%3f clients\", or may be empty, \"\".
 See `set format` for more information.  If no string is given, the
 default label (numerical) is used.

 An explicit tic mark has a third parameter, the level.
 The default is level 0, a major tic.  Level 1 generates a minor tic.
 Labels are never printed for minor tics.  Major and minor tics may be
 auto-generated by the program or specified explicitly by the user.
 Tics with level 2 and higher must be explicitly specified by the user, and
 take priority over auto-generated tics.  The size of tics marks at each
 level is controlled by the command `set tics scale`.

 Examples:
       set xtics (\"low\" 0, \"medium\" 50, \"high\" 100)
       set xtics (1,2,4,8,16,32,64,128,256,512,1024)
       set ytics (\"bottom\" 0, \"\" 10, \"top\" 20)

       set ytics (\"bottom\" 0, \"\" 10 1, \"top\" 20)

 In the second example, all tics are labeled.  In the third, only the end
 tics are labeled.  In the fourth, the unlabeled tic is a minor tic.

 Normally if explicit tics are given, they are used instead of auto-generated
 tics. Conversely if you specify `set xtics auto` or the like it will erase
 any previously specified explicit tics. You can mix explicit and auto-
 generated tics by using the keyword `add`, which must appear before
 the tic style being added.

 Example:
       set xtics 0,.5,10
       set xtics add (\"Pi\" 3.14159)

 This will automatically generate tic marks every 0.5 along x, but will
 also add an explicit labeled tic mark at pi.

 However they are specified, tics will only be plotted when in range.

 Format (or omission) of the tic labels is controlled by `set format`, unless
 the explicit text of a label is included in the `set xtics (\"<label>\")` form.

 Minor (unlabeled) tics can be added automatically by the `set mxtics`
 command, or at explicit positions by the `set xtics (\"\" <pos> 1, ...)` form.

 The appearance of the tics (line style, line width etc.) is determined by the
 border line (see `set border`), even if the tics are drawn at the axes.

Subtopics available for xtics:
    rangelimited      timedata"
	  ))


@annot.doc:doc
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

 Example:
 @b(Пример использования:)

 @begin[lang=lisp](gnuplot)
 #Перемещает xtics более плотно к графику. 
       set xtics offset 0,graph 0.05
 @end(code)
"
(defun valid-offset (offset)
  (labels ((cons-with-key-number (key-lst)
	     (lambda (var)
	       (or (numberp var)
		   (and
		    (consp var)
		    (member (car var) key-lst)
		    (numberp (cdr var)))))))
    (and (every (cons-with-key-number '(:first :second :graph :screen :character)) offset)
	 (<= 2 (length offset) 3))))

@annot.doc:doc
"@b(Пример использования:)
@begin[lang=lisp](code)
 (valid-step-freq :autofreq) => nil
 (valid-step-freq '(1 . 2))  =>  [Condition of type TYPE-ERROR]
 (valid-step-freq 10)        => T
 (valid-step-freq '(10.0 20.0 110.0))  => T
 (valid-step-freq '(10.0 20.0 )) => T
@end(code)
"
(defun valid-step-freq (step-freq)
  (cond
    ((numberp step-freq))        ;;;; <incr>
    ((and (consp step-freq)      ;;;; <start>, <incr> {,<end>}
	  (every #'numberp step-freq)
	  (<= 2 (length step-freq) 3)))))

(defun valid-labels (label)
  (cond
    ((and (consp label)
	  (every
	   #'(lambda (el) (and (stringp (first el)) (numberp (second el))))
	   label)))))

@annot.doc:doc
"@b(Описание:) valid-colorspec проверка на корректность.

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
(defun valid-colorspec (colorspec)
  (cond
    ((and (stringp  colorspec) (gethash colorspec *color-names*) T))
    ((and (integerp colorspec) (<= 0 #xffFFffFF)) T)
    ((and (consp colorspec) (<= 3 (length colorspec) 4)
	  (every
	   #'(lambda (el)
	       (and (integerp el) (<= 0 el #xff)))
	   colorspec)))))

@export
@annot.doc:doc
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

@b(Описание:) gnuplot.
 Syntax:
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
       show xtics"
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
  (when (stringp font) (format stream " font ~S" font))
  (when (and (consp font)
	     (stringp (first font))
	     (numberp (second font)))
    (format stream " font \"~A,~A\"" (first font) (second font)))
  (symbols->stream stream enhanced numeric-timedate-geographic rangelimited)
  (when (stringp textcolor) (format stream " textcolor ~S" textcolor))
  (when (integerp textcolor) (format stream " textcolor \"0x~2,'0X\"" textcolor))
  (when (consp textcolor) (format stream " textcolor \"0x~{~2,'0X~}\"" textcolor)))



		    
@export
@annot.doc:doc
"  
 Many commands allow you to specify a linetype with an explicit color.

 Syntax:

       ... {linecolor | lc} {\"colorname\" | <colorspec> | <n>}
       ... {textcolor | tc} {<colorspec> | {linetype | lt} <n>}

 where <colorspec> has one of the following forms:

       rgbcolor \"colorname\"    # e.g. \"blue\"
       rgbcolor \"0xRRGGBB\"     # string containing hexadecimal constant
       rgbcolor \"0xAARRGGBB\"   # string containing hexadecimal constant
       rgbcolor \"#RRGGBB\"      # string containing hexadecimal in x11 format
       rgbcolor \"#AARRGGBB\"    # string containing hexadecimal in x11 format
       rgbcolor <integer val>  # integer value representing AARRGGBB
       rgbcolor variable       # integer value is read from input file
       palette frac <val>      # <val> runs from 0 to 1
       palette cb <value>      # <val> lies within cbrange
       palette z
       variable                # color index is read from input file
       bgnd                    # background color
       black

 The \"<n>\" is the linetype number the color of which is used, see `test`.

 \"colorname\" refers to one of the color names built in to gnuplot. For a list
 of the available names, see `show colornames`.

 Hexadecimal constants can be given in quotes as \"#RRGGBB\" or \"0xRRGGBB\", where
 RRGGBB represents the red, green, and blue components of the color and must be
 between 00 and FF.  For example, magenta = full-scale red + full-scale blue
 could be represented by \"0xFF00FF\", which is the hexadecimal representation of
 (255 << 16) + (0 << 8) + (255).

 \"#AARRGGBB\" represents an RGB color with an alpha channel (transparency) 
 value in the high bits. An alpha value of 0 represents a fully opaque color;
 i.e., \"#00RRGGBB\" is the same as \"#RRGGBB\".  An alpha value of 255 (FF)
 represents full transparency. `Note`: This convention for the alpha channel
 is backwards from that used by the \"with rgbalpha\" image plot mode in earlier
 versions of gnuplot.

 The color palette is a linear gradient of colors that smoothly maps a
 single numerical value onto a particular color.  Two such mappings are always
 in effect. `palette frac`  maps a fractional value between 0 and 1 onto the
Press return for more: 
 full range of the color palette.  `palette cb` maps the range of the color
 axis onto the same palette.  See `set cbrange`.  See also `set colorbox`.
 You can use either of these to select a constant color from the current
 palette.

 \"palette z\" maps the z value of each plot segment or plot element into the
 cbrange mapping of the palette. This allows smoothly-varying color along a
 3d line or surface. It also allows coloring 2D plots by palette values read
 from an extra column of data (not all 2D plot styles allow an extra column).
 There are two special color specifiers: `bgnd` for background color and `black`."
(defun help-colorspec (&optional (stream t))
  (format stream "~a"  (documentation 'help 'function)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

"set terminal ~A fontscale ~A size ~A~A,~A~A~%"
"set termoption enhanced~%"
"set output \"~A\"~%"
"set key ~A~%"
"set title \"~A\"~%"
"set xrange  ~A~%"
"set tics ~A~%"
"set mxtics ~A~%"
