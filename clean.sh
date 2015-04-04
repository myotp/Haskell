#!/bin/sh

find . -maxdepth 1 -type f ! -name "*.*" -exec rm {} \;
rm -f *.o
rm -f *.hi
