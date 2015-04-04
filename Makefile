compile:
	ghc -O2 -threaded -rtsopts -eventlog labA1.hs
clean:
	@find . -maxdepth 1 -type f ! -name "*.*" ! -name "Makefile" -exec rm {} \;
	@rm -f *.o
	@rm -f *.hi
	@rm -f *~
