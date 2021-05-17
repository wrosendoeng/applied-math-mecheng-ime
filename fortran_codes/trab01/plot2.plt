# gnuplot -c plot.plt ARG1
set terminal pngcairo
filename = ARG1
set output "pictures/ex_18_01.png"
set key top left
set xlabel "x"
set ylabel "f(x)"
set title "Exercise 18 from page 556 - f(x)"
plot filename using 1:2 title "f(x)" lt -1 with lines
