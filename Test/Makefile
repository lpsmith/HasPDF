debug:
	ghc -o test -XForeignFunctionInterface -cpp -Wall -funbox-strict-fields  -fglasgow-exts  -O2 -hidir interfaces -odir bin -optc-I../c -i.. ../c/metrics.c ../c/conversion.c --make test.hs

profile:
	ghc -o test -prof -auto-all -XForeignFunctionInterface -cpp -Wall -funbox-strict-fields  -fglasgow-exts  -O2 -hidir interfaces -odir bin -optc-I../c -i..  ../c/metrics.c ../c/conversion.c --make test.hs
	
runprof:
	./test +RTS -p -hc
		
demo:
	ghc -o test -fglasgow-exts  -O2 --make test.hs
	
buildafm:
	ghc -o afm --make AFMParser.hs
	
metrics:
	@./afm Courier-Bold.afm

clean:
	rm -f test
	rm -f afm
	rm -f *.o
	rm -f *.hi
	rm -f *.exe
	rm -f test.prof
	rm -f *.pdf
	rm -rf bin
	rm -rf interfaces
	rm -rf c
	mkdir bin
	mkdir c
	mkdir interfaces
	
