set term pdfcairo enhanced font ",8" size 10,6
set datafile separator ","

set style data lines
set style line 1 linetype 1 lc rgb "#000000"  lw 2.5 # Black line 

set style line 2 linetype 1 lc rgb "#cc3340"  lw 2.5 # Classification 
set style line 3 linetype 1 lc rgb "#cc3340" dt 4 lw 2.5 # Classification
set style line 4 linetype 1 lc rgb "#cc3340" dt 2 lw 2.5 # Classification

set style line 5 linetype 1 lc rgb "#00a0b0"  lw 2.5 # Regression 
set style line 6 linetype 1 lc rgb "#00a0b0" dt 4  lw 2.5 # Regression 
set style line 7 linetype 1 lc rgb "#00a0b0" dt 2 lw 2.5 # Regression, Size opt


############## Figure: Long Term
set out 'CumulativeReturns_XGBC.pdf'
set multiplot layout 1,1 margins 0.06,0.95,0.08,0.98 spacing 0,0.02 
set grid lt -1 lw 0.05 dt 3
set key left spacing 1.5 font ",11" 
set xdata time; 
set timefmt '%Y-%m-%d'
set format x '%Y'
set style fill transparent solid 0.35
set tics front
set ytics font ",14"
set yrange[0:25]
set xrange["1987-01-30":"2021-12-31"]
set xtics font ",14"
set ylabel "Log Cumulative Returns" font ", 14"
set xtics
set key maxrows 2 font ",14"

plot 'CumulativeReturns.csv'  u 1:2 linestyle 5 title "Optimal - Equally Weighted",\
                                '' u 1:3 linestyle 2 title "Benchmark - Equally Weighted",\
                                '' u 1:4 linestyle 6 title "Optimal - Value Weighted",\
                                '' u 1:5 linestyle 3 title "Benchmark - Value Weighted"

unset multiplot

set out 'CumulativeReturns_OLS.pdf'
set multiplot layout 1,1 margins 0.06,0.95,0.08,0.98 spacing 0,0.02 
set grid lt -1 lw 0.05 dt 3
set key left spacing 1.5 font ",11" 
set xdata time; 
set timefmt '%Y-%m-%d'
set format x '%Y'
set style fill transparent solid 0.35
set tics front
set ytics font ",14"
set yrange[0:25]
set xrange["1987-01-30":"2021-12-31"]
set xtics font ",14"
set ylabel "Log Cumulative Returns" font ", 14"
set xtics
set key maxrows 2 font ",14"

plot 'CumulativeReturns_OLS.csv'  u 1:2 linestyle 5 title "Optimal - Equally Weighted",\
                                '' u 1:3 linestyle 2 title "Decile - Equally Weighted",\
                                '' u 1:4 linestyle 6 title "Optimal - Value Weighted",\
                                '' u 1:5 linestyle 3 title "Decile - Value Weighted"

unset multiplot


set out 'CumulativeReturns_XGBC_TXCosts.pdf'
set multiplot layout 1,1 margins 0.06,0.95,0.08,0.98 spacing 0,0.02 
set grid lt -1 lw 0.05 dt 3
set key left spacing 1.5 font ",11" 
set xdata time; 
set timefmt '%Y-%m-%d'
set format x '%Y'
set style fill transparent solid 0.35
set tics front
set yrange[-0.5:7]
set ylabel "Log Cumulative Returns" font ", 14"
set xtics
set xrange["2000-01-30":"2021-12-31"]
set key maxrows 2 font ",14"

plot 'CumulativeReturns_TxCosts.csv'  u 1:2 linestyle 5 title "Optimal - Equally Weighted",\
                                '' u 1:3 linestyle 2 title "Benchmark - Equally Weighted",\
                                '' u 1:4 linestyle 6 title "Optimal - Value Weighted",\
                                '' u 1:5 linestyle 3 title "Benchmark - Value Weighted"

unset multiplot


