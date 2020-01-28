;;;; set-xrange.lisp

(in-package #:gnuplot)

;;;;(require :cl-annot)
(annot:enable-annot-syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
@annot.doc:doc
"xrange
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
(defun set-*range (axis &key (stream t) min max no-reverse reverse no-writeback writeback no-extend extend)
  (assert (member axis '("x" "y" "z" "x2" "y2" "cb" "r" "t" "u" "v") :test #'string=))
  (format stream "set")
  (when (and (null min) (null max)) (format stream " ~Arange []"      axis))
  (when (and (null min)       max ) (format stream " ~Arange [:~A]"   axis max))
  (when (and       min  (null max)) (format stream " ~Arange [~A:]"   axis min ))
  (when (and       min        max)  (format stream " ~Arange [~A:~A]" axis min max))
  (when no-reverse   (format stream " noreverse"))
  (when reverse      (format stream " reverse"))
  (when no-writeback (format stream " nowriteback"))
  (when writeback    (format stream " writeback"))
  (when no-extend    (format stream " noextend"))
  (when extend       (format stream " extend")))

@export
@annot.doc:doc
" set xrange [{{<min>}:{<max>}}] {{no}reverse} {{no}writeback} {{no}extend} | restore
 (set-*range-restore \"z\")
"
(defun set-*range-restore (axis &key stream)
  (assert (member axis '("x" "y" "z" "x2" "y2" "cb" "r" "t" "u" "v") :test #'string=))
  (format stream "set ~Arange" axis))
