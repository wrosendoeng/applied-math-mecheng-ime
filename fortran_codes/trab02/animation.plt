# gnuplot -c animation.plt ARG1
set terminal png font 'arial, 12'
filename = ARG1
set border 4095
set samples 25, 25
set isosamples 20, 20
set grid
# Defining name for output
set output 'heatequation.png'
# Set ranges for x and y
set xrange [0.00000:6.00000] 
set yrange [ 0.000000 : 10.00000 ] 
set zrange [ * : * ] 
# Axis label and title
set xlabel 'x (m)' offset character 0,-3,0
set ylabel 't (s)' offset character 3,0,0
set zlabel 'u(x,t)' offset character -3,0,0
set title 'Heat Equation: u_t - alpha*u_{xx} = 0' font 'Arial, 14'
# Disappear with bottom surface
set hidden3d
# 3D Plotting from data file with pm3d
splot filename title "Heat PDE" with lines
