;;;; help.lisp

(in-package #:gnuplot)

(annot:enable-annot-syntax)

@export
@annot.doc:doc
" @b(Описание:) help-set-grid возврвщает помощь по команде gnuplot
@begin[lang=gnuplot](code)
  set grid
@end(code)

 @b(Gnuplot:)
@begin[lang=gnuplot](code)
 The `set grid` command allows grid lines to be drawn on the plot.

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
 partial box is drawn around the plot---see `set border`.
@end(code)
"
(defun help-set-grid (&optional (stream t))
  (format stream "~a"  (documentation 'help-set-grid 'function)))

@export
@annot.doc:doc
" @b(Описание:) help-set-polar выводит помощь по команде gnuplot

@begin[lang=gnuplot](code)
 set polar
@end(code)

 @b(Gnuplot:) 
@begin[lang=gnuplot](code)
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
@end(code)
"
(defun help-set-polar (&optional (stream t))
  (format stream "~a"  (documentation 'help-set-polar 'function)))

@export
@annot.doc:doc
"@begin[lang=gnuplot](code)
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
@end(code)
"
(defun help-set-*range (&optional (stream t))
  (format stream "~a"  (documentation 'help-set-*range 'function)))

@export
@annot.doc:doc
" @b(Описание:) help-set-*tics Помощь по командам gnuplot:
 set xtics, set ytics, set ztics, set x2tics, set y2tics 

@begin[lang=gnuplot](code)
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
    rangelimited      timedata
@end(code)
"
(defun help-set-*tics (&optional (stream t))
  (format stream "~a"  (documentation 'help-set-*tics 'function)))

@export
@annot.doc:doc
"
@begin[lang=gnuplot](code)
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
 There are two special color specifiers: `bgnd` for background color and `black`.
@end(code)"
(defun help-colorspec (&optional (stream t))
  (format stream "~a"  (documentation 'help-colorspec 'function)))

@export
@annot.doc:doc
"@begin[lang=gnuplot](code)
 The `set key` command enables a key (or legend) containing a title and a
 sample (line, point, box) for each plot in the graph. The key may be turned off
 by requesting `set key off` or `unset key`.  Individual key entries may be
 turned off by using the `notitle` keyword in the corresponding plot command.
 The text of the titles is controlled by the `set key autotitle` option or by
 the `title` keyword of individual `plot` and `splot` commands.
 See `plot title` for more information.

 Syntax:
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

 Elements within the key are stacked according to `vertical` or `horizontal`.
 In the case of `vertical`, the key occupies as few columns as possible.  That
 is, elements are aligned in a column until running out of vertical space at
 which point a new column is started.  The vertical space may be limited using
 'maxrows'.  In the case of `horizontal`, the key occupies as few rows as
 possible.  The horizontal space may be limited using 'maxcols'.

 By default the key is placed in the upper right inside corner of the graph.
 The keywords `left`, `right`, `top`, `bottom`, `center`, `inside`, `outside`,
 `lmargin`, `rmargin`, `tmargin`, `bmargin` (, `above`, `over`, `below` and
 `under`) may be used to automatically place the key in other positions of the
 graph.  Also an `at <position>` may be given to indicate precisely where the
 plot should be placed.  In this case, the keywords `left`, `right`, `top`,
 `bottom` and `center` serve an analogous purpose for alignment.
 For more information, see `key placement`.

 Justification of the plot titles within the key is controlled by `Left` or
 `Right` (default).  The text and sample can be reversed (`reverse`) and a
 box can be drawn around the key (`box {...}`) in a specified `linetype`
 and `linewidth`, or a user-defined `linestyle`.

 The text in the key is set in `enhanced` mode by default, this can be changed 
 with the `{no}enhanced` option, also independently for the key title only and
 for each individual plot.

 By default the key is built up one plot at a time. That is, the key symbol and
 title are drawn at the same time as the corresponding plot.  That means newer
 plots may sometimes place elements on top of the key.  `set key opaque` causes
 the key to be generated after all the plots.  In this case the key area is 
 filled with background color and then the key symbols and titles are written.
 Therefore the key itself may obscure portions of some plot elements.
 The default can be restored by `set key noopaque`.

 By default the first plot label is at the top of the key and successive labels
 are entered below it. The `invert` option causes the first label to be placed
 at the bottom of the key, with successive labels entered above it. This option
 is useful to force the vertical ordering of labels in the key to match the
 order of box types in a stacked histogram.

 The <height_increment> is a number of character heights to be added to or
 subtracted from the height of the key box.  This is useful mainly when you are
 putting a box around the key and want larger borders around the key entries.

 An overall title can be put on the key (`title \"<text>\"`)---see also `syntax`
 for the distinction between text in single- or double-quotes. The justification
 of the title defaults to center and can be changed by the keywords `right` or
 `left`

 The defaults for `set key` are `on`, `right`, `top`, `vertical`, `Right`,
 `noreverse`, `noinvert`, `samplen 4`, `spacing 1`, `notitle`, and
 `nobox`.  The default <linetype> is the same as that used for the plot
 borders.  Entering `set key default` returns the key to its default
 configuration.

 The key is drawn as a sequence of lines, with one plot described on each
 line.  On the right-hand side (or the left-hand side, if `reverse` is
 selected) of each line is a representation that attempts to mimic the way the
 curve is plotted.  On the other side of each line is the text description
 (the line title), obtained from the `plot` command.  The lines are vertically
 arranged so that an imaginary straight line divides the left- and right-hand
 sides of the key.  It is the coordinates of the top of this line that are
 specified with the `set key` command.  In a `plot`, only the x and y
 coordinates are used to specify the line position.

 When using the TeX/LaTeX group of terminals or terminals in which formatting
 information is embedded in the string, `gnuplot` can only estimate the width
 of the string for key positioning.  If the key is to be positioned at the
 left, it may be convenient to use the combination `set key left Left reverse`.


Subtopics available for key:
    3D                autotitle         examples          fixed
    multiple          placement         samples           splot
@end(code)"
(defun help-set-key (&optional (stream t))
    (format stream "~a"  (documentation 'help-set-key 'function)))


@export
@annot.doc:doc
"@begin[lang=gnuplot](code)
 Minor tic marks along the x axis are controlled by `set mxtics`.  They can be
 turned off with `unset mxtics`.  Similar commands control minor tics along
 the other axes.

 Syntax:
       set mxtics {<freq> | default}
       unset mxtics
       show mxtics

 The same syntax applies to `mytics`, `mztics`, `mx2tics`, `my2tics`, `mrtics`,
 `mttics` and `mcbtics`.

 <freq> is the number of sub-intervals (NOT the number of minor tics) between
 major tics (the default for a linear axis is either two or five
 depending on the major tics, so there are one or four minor
 tics between major tics). Selecting `default` will return the number of minor
 ticks to its default value.

 If the axis is logarithmic, the number of sub-intervals will be set to a
 reasonable number by default (based upon the length of a decade).  This will
 be overridden if <freq> is given.  However the usual minor tics (2, 3, ...,
 8, 9 between 1 and 10, for example) are obtained by setting <freq> to 10,
 even though there are but nine sub-intervals.

 To set minor tics at arbitrary positions, use the (\"<label>\" <pos> <level>,
 ...) form of `set {x|x2|y|y2|z}tics` with <label> empty and <level> set to 1.

 The `set m{x|x2|y|y2|z}tics` commands work only when there are uniformly
 spaced major tics.  If all major tics were placed explicitly by
 `set {x|x2|y|y2|z}tics`, then minor tic commands are ignored.  Implicit
 major tics and explicit minor tics can be combined using
 `set {x|x2|y|y2|z}tics` and `set {x|x2|y|y2|z}tics add`.

 Examples:
       set xtics 0, 5, 10
       set xtics add (7.5)
       set mxtics 5
 Major tics at 0,5,7.5,10, minor tics at 1,2,3,4,6,7,8,9
       set logscale y
       set ytics format \"\"
       set ytics 1e-6, 10, 1
       set ytics add (\"1\" 1, \".1\" 0.1, \".01\" 0.01, \"10^-3\" 0.001, \
                      \"10^-4\" 0.0001)
       set mytics 10
 Major tics with special formatting, minor tics at log positions

 By default, minor tics are off for linear axes and on for logarithmic axes.
 They inherit the settings for `axis|border` and `{no}mirror` specified for
 the major tics.  Please see `set xtics` for information about these.
@end(code)"
(defun help-set-m*tics (&optional (stream t))
    (format stream "~a"  (documentation 'help-set-m*tics 'function)))

@export
@annot.doc:doc
"@begin[lang=gnuplot](code)
help set title
 The `set title` command produces a plot title that is centered at the top of
 the plot.  `set title` is a special case of `set label`.

 Syntax:
       set title {\"<title-text>\"} {offset <offset>} {font \"<font>{,<size>}\"}
                 {{textcolor | tc} {<colorspec> | default}} {{no}enhanced}
       show title

 If <offset> is specified by either x,y or x,y,z the title is moved by the
 given offset.  It may be preceded by `first`, `second`, `graph`, `screen`,
 or `character` to select the coordinate system.  See `coordinates` for
 details.  By default, the `character` coordinate system is used.  For
 example, \"`set title offset 0,-1`\" will change only the y offset of the
 title, moving the title down by roughly the height of one character.  The
 size of a character depends on both the font and the terminal.

 <font> is used to specify the font with which the title is to be written;
 the units of the font <size> depend upon which terminal is used.

 `textcolor <colorspec>` changes the color of the text. <colorspec> can be a
 linetype, an rgb color, or a palette mapping. See help for `colorspec` and
 `palette`.

 `noenhanced` requests that the title not be processed by the enhanced text
 mode parser, even if enhanced text mode is currently active.

 `set title` with no parameters clears the title.

 See `syntax` for details about the processing of backslash sequences and
 the distinction between single- and double-quotes.
@end(code)
"
(defun help-set-title (&optional (stream t))
    (format stream "~a"  (documentation 'help-set-m*tics 'function)))

