# gnuplot -c plot.plt ARG1 ARG2
set terminal pdfcairo
filename = ARG1
n = ARG2 + 0
set output sprintf("ex18_n_%s.pdf",ARG2)
set xlabel "x"
set ylabel "f(x)"
set title sprintf("Exercise 18 from page 556 - n = %.f",n)
plot filename using 1:2 title "f(x)" lt -1 with lines, \
     filename using 1:3 title "fft(x)" lt 4 with dots