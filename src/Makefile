.PHONY: all clean docker-ghci docker docker-ghci-debug docker-test test tensor tensorkernel xperm

LIBDIR = ../canonicalize
LDOPTS = -lstdc++ -lcanonicalize -L$(LIBDIR)
#LDOPTS = -lstdc++ -L$(LIBDIR)

all: xperm tensor tensorkernel # libcanonicalize.so

xperm: external/xperm_new.cc external/xperm_new.h
	#g++ external/xperm_new.cc -fPIC -shared -o external/libxperm.so
	g++ -c external/xperm_new.cc -o external/libxperm.o
	ar crv external/libxperm.a external/libxperm.o

HSMODULES = Eval.hs \
	    Core.hs \
	    Tensor.hs \
	    Check.hs \
	    Frontend/LexTensor.hs \
	    Frontend/ParTensor.hs \
	    RenderCalc.hs \
	    Transform.hs \
	    CalcFromExpr.hs

tensorkernel: Kernel.hs $(HSMODULES)
	ghc --make $< $(LDOPTS) -static -o tensorkernel

tensor: Main.hs XPerm.hs $(HSMODULES)
	ghc --make $< $(LDOPTS) -static -o tensor

test: Test.hs $(HSMODULES)
	runhaskell RunTest.hs --ghc-arg="$(LDOPTS)"
	
docker: Main.hs $(HSMODULES)
	docker run --rm -v $(PWD)/../:/bnfctensor -w /bnfctensor/src bnfctensor:latest make tensor

docker-test: Test.hs $(HSMODULES)
	docker run --rm -v $(PWD)/../:/bnfctensor -w /bnfctensor/src bnfctensor:latest make test

docker-ghcid:
	docker run --rm -it -v $(PWD)/../:/bnfctensor -v $(HOME)/.ghc/ghci_history:/root/.ghc/ghci_history -w /bnfctensor/src bnfctensor:latest ghcid "--command=ghci -lstdc++ -fobject-code" 

docker-ghci:
	#docker run --rm -it -v $(PWD)/../:/bnfctensor -v $(HOME)/.ghc/ghci_history:/root/.ghc/ghci_history -w /bnfctensor/src bnfctensor:latest ghci -lstdc++ -fobject-code
	docker run --rm -it -v $(PWD)/../:/bnfctensor -v $(HOME)/.ghc/ghci_history:/root/.ghc/ghci_history -w /bnfctensor/src bnfctensor:latest ghci -L../canonicalize/ -lcanonicalize -lstdc++ -fobject-code

docker-ghci-debug:
	docker run --rm -it -v $(PWD)/../:/bnfctensor -v $(HOME)/.ghc/ghci_history:/root/.ghc/ghci_history -w /bnfctensor bnfctensor:debug ghci -fexternal-interpreter -prof

clean:
	-rm -f tensor *.hi *.o Frontend/*.hi Frontend/*.o

LEXER = LexTensor.hs
PARSER = ParTensor.hs

VPATH = Frontend

Frontend/%.hs: %.y
	happy -gca $<

Frontend/%.hs: %.x
	alex -g $<

Frontend/ParTensor.y Frontend/LexTensor.x: Tensor.cf
	bnfc -p Frontend --ghc $<


VPATH = ../canonicalize

libcanonicalize.so: src/lib.rs
	$(MAKE) -C ../canonicalize

