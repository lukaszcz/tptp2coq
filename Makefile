all: tptp2coq

tptp2coq: tptp2coq.hs
	ghc tptp2coq.hs

clean:
	rm *.o *.hi tptp2coq
