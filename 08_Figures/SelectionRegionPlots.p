#set term pdfcairo enhanced font ",8" size 10,6
set term pdfcairo enhanced font ",8" size 12,6
set datafile separator ","

set style fill transparent border

set out 'ClassificationRegion.pdf'
set multiplot layout 1,2 margins 0.06,0.95,0.08,0.98 spacing 0.02,0.02 
set grid lt -1 lw 0.05 dt 3 front
set key right spacing 1.5 font ",14" 
set tics front
set xtics font ",14"
set ytics font ",14"
set xtics
set xrange [0:1]
set yrange [0:1]
set key maxrows 2 font ",14"
set ylabel 'P(Winner)' font ",15"
set xlabel 'P(Loser)' font ",15"



f(x) = 1 - x
g(x) = x
plot 'SelectionRegion_Classification.csv'  u 2:5 with filledcurves y1 fc rgb  "#00a0b0" fillstyle solid 0.5 border lc rgb "black" lw 2  lt 1 notitle,\
                                       ''  u 6:2 with filledcurves x1 fc rgb  "#cc3340" fillstyle solid 0.5 border lc rgb "black" lw 2  lt 1 notitle,\
                                       '' u 2:5 with lines lt -1 notitle,\
                                       '' u 6:2 with lines lt -1 notitle ,\
                                       f(x) with filledcurves xy=1,1 fc rgb "grey" notitle ,\
                                       f(x) with lines lt -1  notitle ,\
                                       g(x) with lines lt -1 dt 2 notitle 
 

set grid lt -1 lw 0.05 dt 3 front
set ytics format ""
set xrange [0:1]
set yrange [0:1]
set key maxrows 2 font ",14"
set ylabel " "

f(x) = 1 - x
g(x) = x
plot 'SelectionRegion_Classification.csv'  u 2:3 with filledcurves y1 fc rgb  "#00a0b0" fillstyle solid 0.5 border lc rgb "black" lw 2  lt 1 notitle,\
                                       ''  u 4:2 with filledcurves x1 fc rgb  "#cc3340" fillstyle solid 0.5 border lc rgb "black" lw 2  lt 1 notitle,\
                                       '' u 2:3 with lines lt -1 notitle,\
                                       '' u 4:2 with lines lt -1 notitle ,\
                                       f(x) with filledcurves xy=1,1 fc rgb "grey" notitle ,\
                                       f(x) with lines lt -1  notitle ,\
                                       g(x) with lines lt -1 dt 2 notitle 
           

          
unset multiplot

set out 'RegressionRegion.pdf'
set multiplot layout 1,2 margins 0.06,0.95,0.08,0.98 spacing 0.02,0.02 
set grid lt -1 lw 0.05 dt 3 front
set key right spacing 1.5 font ",14" 
set tics front
set xtics font ",14"
set ytics font ",14"
set xtics
set xrange [0:3.5]
set yrange [0:1]
set key maxrows 2 font ",14"
set ylabel 'Exp. Return' font ",15"
set xlabel 'Variance' font ",15"


plot 'SelectionRegion_Regression.csv'  u 1:4 with filledcurves y1=1 fc rgb  "#00a0b0" fillstyle solid 0.5 border lc rgb "black" lw 2  lt 1 notitle,\
                                      ''  u 1:5 with filledcurves x1 fc rgb  "#cc3340" fillstyle solid 0.5 border lc rgb "black" lw 2  lt 1 notitle,\
                                       '' u 1:4 with lines lt -1 notitle,\
                                       '' u 1:5 with lines lt -1 notitle
           
set grid lt -1 lw 0.05 dt 3 front
set key right spacing 1.5 font ",14" 
set tics front
unset ytics
set xtics font ",14"
set ytics font ",14"
set xrange [0:3.5]
set yrange [0:1]
set key maxrows 2 font ",14"
set ylabel ""

plot 'SelectionRegion_Regression.csv'  u 1:2 with filledcurves y1=1 fc rgb  "#00a0b0" fillstyle solid 0.5 border lc rgb "black" lw 2  lt 1 notitle,\
                                      ''  u 1:3 with filledcurves x1 fc rgb  "#cc3340" fillstyle solid 0.5 border lc rgb "black" lw 2  lt 1 notitle,\
                                       '' u 1:2 with lines lt -1 notitle,\
                                       '' u 1:3 with lines lt -1 notitle
           
unset multiplot

