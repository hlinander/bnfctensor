all:
	happy -gca ParTensor.y
	alex -g LexTensor.x
	ghc --make TestTensor.hs -o TestTensor

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocTensor.* LexTensor.* ParTensor.* LayoutTensor.* SkelTensor.* PrintTensor.* TestTensor.* AbsTensor.* TestTensor ErrM.* SharedString.* ComposOp.* Tensor.dtd XMLTensor.* Makefile*

test_books := $(wildcard test_suite/*)

test: $(test_books)
	./TestTensor $<

