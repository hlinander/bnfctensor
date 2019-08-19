.PHONY: all clean docker-ghcid docker-ghci docker docker-ghci-debug docker-test test tensor tensorkernel xperm

LIBDIR = ../canonicalize
LDOPTS = -lstdc++ -lcanonicalize -L$(LIBDIR)
#LDOPTS = -lstdc++ -L$(LIBDIR)

all: xperm tensor tensorkernel libcanonicalize.so

xperm: external/xperm_new.cc external/xperm_new.h
	#g++ external/xperm_new.cc -fPIC -shared -o external/libxperm.so
	g++ -c external/xperm_new.cc -o external/libxperm.o
	ar crv external/libxperm.a external/libxperm.o

CANONICALIZE = libcanonicalize.so

HSMODULES = Eval.hs \
	    Core.hs \
	    Tensor.hs \
	    Check.hs \
	    Frontend/LexTensor.hs \
	    Frontend/ParTensor.hs \
	    RenderCalc.hs \
	    Transform.hs \
	    CalcFromExpr.hs

tensorkernel: Kernel.hs $(HSMODULES) $(CANONICALIZE)
	ghc --make $< $(LDOPTS) -static -o tensorkernel -no-keep-hi-files -no-keep-o-files -tmpdir=/tmp -hidir=hi/

tensor: Main.hs XPerm.hs $(HSMODULES) $(CANONICALIZE)
	ghc --make $< $(LDOPTS) -static -o tensor -no-keep-hi-files -no-keep-o-files -tmpdir=/tmp -hidir=hi/

test: Test.hs $(HSMODULES) $(CANONICALIZE)
	LD_PRELOAD="$(LIBDIR)/libcanonicalize.so" runhaskell RunTest.hs
	
docker: Main.hs $(HSMODULES)
	docker run --rm -v $(PWD)/../:/bnfctensor -w /bnfctensor/src bnfctensor:latest make tensor

docker-test: Test.hs $(HSMODULES)
	docker run --rm -v $(PWD)/../:/bnfctensor -w /bnfctensor/src bnfctensor:latest make test

docker-ghcid: $(CANONICALIZE)
	docker run --rm -it -v $(PWD)/../:/bnfctensor -v $(HOME)/.ghc/ghci_history:/root/.ghc/ghci_history -w /bnfctensor/src bnfctensor:latest ghcid "--command=ghci -lstdc++ -fobject-code" 

docker-ghci: $(CANONICALIZE)
	#docker run --rm -it -v $(PWD)/../:/bnfctensor -v $(HOME)/.ghc/ghci_history:/root/.ghc/ghci_history -w /bnfctensor/src bnfctensor:latest ghci -lstdc++ -fobject-code
	docker run --rm -it -v $(PWD)/../:/bnfctensor -v $(HOME)/.ghc/ghci_history:/root/.ghc/ghci_history -w /bnfctensor/src bnfctensor:latest ghci -L../canonicalize/ -lcanonicalize -lstdc++ -fobject-code

docker-ghci-debug:
	docker run --rm -it -v $(PWD)/../:/bnfctensor -v $(HOME)/.ghc/ghci_history:/root/.ghc/ghci_history -w /bnfctensor bnfctensor:debug ghci -fexternal-interpreter -prof

clean:
	-rm -f tensor *.hi *.dyn_hi *.o *.dyn_o Frontend/*.hi Frontend/*.o Frontend/*.hs Frontend/*.x Frontend/*.y
	$(MAKE) -C ../canonicalize clean

docker-clean:
	docker run --rm -v $(PWD)/../:/bnfctensor -w /bnfctensor/src bnfctensor:latest make clean

LEXER = LexTensor.hs
PARSER = ParTensor.hs

VPATH = Frontend

%.hs: %.y
	happy -gca $<

%.hs: %.x
	alex -g $<

Frontend/ParTensor.y Frontend/LexTensor.x: Tensor.cf
	bnfc -p Frontend --ghc $<


VPATH = ../canonicalize

libcanonicalize.so: src/lib.rs
	$(MAKE) -C ../canonicalize

