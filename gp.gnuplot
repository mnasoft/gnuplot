set terminal pdfcairo fontscale 1 size 30cm,30cm
set termoption enhanced
set output "gp.pdf"

set key below
set title "GnuPlot Graph"

set xrange  [*:*]
set yrange  [*:*]
set x2range [*:*]
set y2range [*:*]

set tics out
set tics scale 2,1

set xtics nomirror
set ytics nomirror

set mxtics 5
set mytics 5

f_1_3(x) = a_1_3 + b_1_3*x + c_1_3*x**2 + d_1_3*x**3 + e_1_3*x**4 + f_1_3*x**5
fit f_1_3(x) "gp.txt" using 1:3 via a_1_3,b_1_3,c_1_3,d_1_3,e_1_3,f_1_3
f_1_4(x) = a_1_4 + b_1_4*x + c_1_4*x**2 + d_1_4*x**3 + e_1_4*x**4 + f_1_4*x**5
fit f_1_4(x) "gp.txt" using 1:4 via a_1_4,b_1_4,c_1_4,d_1_4,e_1_4,f_1_4

f_1_2(x) = a_1_2 + b_1_2*x + c_1_2*x**2 + d_1_2*x**3 + e_1_2*x**4 + f_1_2*x**5
fit f_1_2(x) "gp.txt" using 1:2 via a_1_2,b_1_2,c_1_2,d_1_2,e_1_2,f_1_2
f_1_5(x) = a_1_5 + b_1_5*x + c_1_5*x**2 + d_1_5*x**3 + e_1_5*x**4 + f_1_5*x**5
fit f_1_5(x) "gp.txt" using 1:5 via a_1_5,b_1_5,c_1_5,d_1_5,e_1_5,f_1_5


plot\
"gp.txt" using 1:3 pt 8 ps 2 lt -1 lw 3  title "V_1", f_1_3(x) lt -1 lw 3 title "",\
"gp.txt" using 1:4 pt 4 ps 2 lt -1 lw 3  title "A_1", f_1_4(x) lt -1 lw 3 title "",\
"gp.txt" using 1:2 axis x1y2 pt 6 ps 2 lt -1 lw 3  title "S_1", f_1_2(x) axis x1y2 lt -1 lw 3 title "",\
"gp.txt" using 1:5 axis x1y2 pt 10 ps 2 lt -1 lw 3  title "Y_1", f_1_5(x) axis x1y2 lt -1 lw 3 title ""
set grid xtics ytics mxtics mytics lt -1 lw 3, lt -1 lw 1

set output "gp.pdf"; replot; set output "0.pdf"