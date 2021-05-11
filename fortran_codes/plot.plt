# gnuplot -c plot.plt ARG1 ARG2
set terminal png
set output sprintf("pictures/ex18_n_%s.png",ARG2)
set key top left
set xlabel "x"
set ylabel "f(x)"
set title sprintf("Exercise 18 from page 556 - n = %s",ARG2)
plot ARG1 using 1:2 title "f(x)" lc rgb "red" dt 1 lw 2 with lines, \
     ARG1 using 1:3 title "fft(x)" lc rgb "blue" dt 2 lw 1 with points