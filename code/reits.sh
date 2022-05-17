#!/bin/bash

echo " Downloading the TTracker data "
echo ""

wget "https://reit.com/data-research/reit-market-data/nareit-t-tracker-quarterly-operating-performance-series" -O temp
grep -Po 'href="\K.*.xlsx?(?=")' temp | perl -ne 'print "https://reit.com$_"' | wget -i - -O ../data/ttracker.xlsx

rm temp

echo ""
echo " Done! "
echo ""
echo " btw you're looking fine today "
echo " hope that's okay to say "
echo ""

 
