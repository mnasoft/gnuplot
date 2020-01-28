;;;; gnuplot_01.lisp

(in-package #:gnuplot)

(annot:enable-annot-syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-xtics ()
  "{axis | border} "
  )

(defun set-xtics-axis ()
  "{axis | border}"  
  )

(defun set-xtics-border ()
  "{axis | border}"
)

(defun set-xtics-no-mirror ()
  "{{no}mirror}"
  )

(defun set-xtics-mirror ()
  "{{no}mirror}"
  )

(defun set-xtics-in ()
  "{in | out}"
  )

(defun set-xtics-out ()
  "{in | out}"
  )

(defun set-xtics-scale ( major &optional minor)
  "{scale {default | <major> {,<minor>}}}"
  )

(defun set-xtics-scale-default ()
  "{scale {default | <major> {,<minor>}}}"
  )

(defun set-xtics-no-rotate ()
  "{{no}rotate {by <ang>}}"
  )

(defun set-xtics-rotate ()
  "{{no}rotate {by <ang>}}"
  )

(defun set-xtics-rotate-by (ang)
  "{{no}rotate {by <ang>}}"
  )

(defun set-xtics-offset (offset)
  "{offset <offset> | nooffset}"
  )

(defun set-xtics-no-offset ()
  "{offset <offset> | nooffset}"
  )

;;;;;;;;;;;;;;;;;;;;

(defun set-xtics-left ()
  "{left | right | center | autojustify}"
  )

(defun set-xtics-right ()
  "{left | right | center | autojustify}"
  )

(defun set-xtics-center ()
  "{left | right | center | autojustify}"
  )

(defun set-xtics-autojustify ()
  "{left | right | center | autojustify}"
  )

;;;;;;;;;;;;;;;;;;;;

(defun set-xtics-autojustify ()
  "{add}"
  )

;;;;;;;;;;;;;;;;;;;;

(defun set-xtics-autofreq ()
  "{autofreq | <incr> | <start>, <incr> {,<end>} | ({\"<label>\"} <pos> {<level>} {,{\"<label>\"}...) }"
  )

(defun set-xtics-incr (incr)
  "{autofreq | <incr> | <start>, <incr> {,<end>} | ({\"<label>\"} <pos> {<level>} {,{\"<label>\"}...) }"
  )

(defun set-xtics-start-incr (start incr &optional end)
  "{autofreq | <incr> | <start>, <incr> {,<end>} | ({\"<label>\"} <pos> {<level>} {,{\"<label>\"}...) }"
  )

(defun set-xtics-labels (labels)
  "{autofreq | <incr> | <start>, <incr> {,<end>} | ({\"<label>\"} <pos> {<level>} {,{\"<label>\"}...) }"
  )



gnuplot
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
                  | ({"<label>"} <pos> {<level>} {,{"<label>"}...) }
                 {format "formatstring"} {font "name{,<size>}"} {{no}enhanced}
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


 The explicit ("<label>" <pos> <level>, ...) form allows arbitrary tic
 positions or non-numeric tic labels.  In this form, the tics do not
 need to be listed in numerical order.  Each tic has a
 position, optionally with a label.  Note that the label is
 a string enclosed by quotes.  It may be a constant string, such as
 "hello", may contain formatting information for converting the
 position into its label, such as "%3f clients", or may be empty, "".
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
       set xtics ("low" 0, "medium" 50, "high" 100)
       set xtics (1,2,4,8,16,32,64,128,256,512,1024)
       set ytics ("bottom" 0, "" 10, "top" 20)

       set ytics ("bottom" 0, "" 10 1, "top" 20)

 In the second example, all tics are labeled.  In the third, only the end
 tics are labeled.  In the fourth, the unlabeled tic is a minor tic.

 Normally if explicit tics are given, they are used instead of auto-generated
 tics. Conversely if you specify `set xtics auto` or the like it will erase
 any previously specified explicit tics. You can mix explicit and auto-
 generated tics by using the keyword `add`, which must appear before
 the tic style being added.

 Example:
       set xtics 0,.5,10
       set xtics add ("Pi" 3.14159)

 This will automatically generate tic marks every 0.5 along x, but will
 also add an explicit labeled tic mark at pi.

 However they are specified, tics will only be plotted when in range.

 Format (or omission) of the tic labels is controlled by `set format`, unless
 the explicit text of a label is included in the `set xtics ("<label>")` form.

 Minor (unlabeled) tics can be added automatically by the `set mxtics`
 command, or at explicit positions by the `set xtics ("" <pos> 1, ...)` form.

 The appearance of the tics (line style, line width etc.) is determined by the
 border line (see `set border`), even if the tics are drawn at the axes.

Subtopics available for xtics:
    rangelimited      timedata
