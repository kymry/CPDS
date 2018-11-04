set term pdf
set output "../forward_validation.pdf"
set xlabel "number of clients"
set ylabel "percentage of successfull transactions"
set colorsequence classic

plot "forward.dat" using 1:2 title "forward" with lines, \
     "backward.dat" using 1:2 title "backward" with lines