set terminal pdf color font "Lato,11" linewidth 4 dashed

set pointsize 1
set style line 1 linecolor "#e41a1c" pointtype 5
set style line 2 linecolor "#377eb8" pointtype 7
set style line 3 linecolor "#4daf4a" pointtype 9
set style line 4 linecolor "#984ea3" pointtype 13
set style line 5 linecolor "#ff7f00" pointtype 15

set xlabel "iterations"
set ylabel "time (s)"
set grid
set style increment user

set output "test.pdf"

set datafile separator ','

set key outside autotitle columnhead
set offsets 0.1, 0.1, 0.1, 0.1
plot "test2.csv" using 1:2 with linespoints, "" using 1:3 with linespoints
