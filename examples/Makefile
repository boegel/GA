all: theNumber hello

%: %.hs
	ghc --make -Wall $@

clean:
	rm -f *.hi *.o theNumber hello
