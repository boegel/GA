all: example1 example2

%: GA.hs %.hs
	ghc --make -Wall $@

clean:
	rm -f *.hi *.o example1 example2
