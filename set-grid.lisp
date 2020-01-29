;;;; set-grid.lisp

(in-package #:gnuplot)

(annot:enable-annot-syntax)

(defun symbol-string (sym)
  (mnas-string:replace-all (string-downcase (symbol-name sym)) "-" ""))

(defun symbols->stream (stream &rest rest)
  (map nil (lambda (el) (when el (format stream " ~A" (symbol-string el)))) rest))

@export
@annot.doc:doc
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
(defun set-grid (&key
		   (stream t)
		   xtics  ytics  ztics
                   x2tics y2tics rtics
		   cbtics
                   polar angle
                   layer
                   line-properties-major line-properties-minor)
  (assert (member xtics '(nil :xtics :m-xtics :no-xtics :no-m-xtics ) :test #'eq))
  (assert (member ytics '(nil :ytics :m-ytics :no-ytics :no-m-ytics ) :test #'eq))
  (assert (member ztics '(nil :ztics :m-ztics :no-ztics :no-m-ztics ) :test #'eq))
  
  (assert (member x2tics '(nil :x2tics :m-x2tics :no-x2tics :no-m-x2tics ) :test #'eq))
  (assert (member y2tics '(nil :y2tics :m-y2tics :no-y2tics :no-m-y2tics ) :test #'eq))
  (assert (member rtics  '(nil :rtics :m-rtics :no-rtics :no-m-rtics )     :test #'eq))

  (assert (member cbtics '(nil :cbtics :m-cbtics :no-cbtics :no-m-cbtics ) :test #'eq))

  (assert (member layer '(nil :layer-default :front :back ) :test #'eq))

  (format stream "set grid")
  (symbols->stream stream xtics ytics ztics x2tics y2tics rtics cbtics layer)

  (when (and polar (null angle))  (format stream " polar"))
  (when angle                     (format stream " polar ~A" angle))
  
  (when (and line-properties-major (null line-properties-minor))
    (format stream " ~A" line-properties-major))
  
  (when (and line-properties-major line-properties-minor)
    (format stream " ~A, ~A" line-properties-major line-properties-minor)))

(set-grid :xtics :xtics :ztics :ztics :layer :back)

@export
@annot.doc:doc
"
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
(defun set-polar (&key (stream t)) (format stream "set polar"))

(defun unset-polar (&key (stream t)) (format stream "unset polar"))

(defun show-polar (&key (stream t)) (format stream "show polar"))

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



