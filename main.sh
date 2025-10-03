#/usr/bin/sh
parallel --colsep ' ' Rscript main.R {1} {2} {3} :::: data/args.txt

