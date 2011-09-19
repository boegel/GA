all: example1 example2 example3

%: %.hs
	ghc --make -Wall $@

clean:
	rm -f *.hi *.o example1 example2 example3
