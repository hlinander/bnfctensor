.PHONY: all clean docker-ghcid docker-ghci docker docker-ghci-debug docker-test test

LIBDIR = ../canonicalize
LDOPTS = -lstdc++ -lcanonicalize -L$(LIBDIR)
GHCOPTS = -static -hidir=build/ -odir=build/ -g
# -no-keep-hi-files -no-keep-o-files -fhpc 

UID = $(shell id -u)
GID = $(shell id -g)
DOCKERLIB = -e "LD_LIBRARY_PATH=/bnfctensor/canonicalize" 
DOCKERDEBUG = --cap-add=SYS_PTRACE --security-opt seccomp=unconfined
DOCKER = --rm -it -u $(UID):$(GID) -v $(PWD)/../:/bnfctensor -w /bnfctensor/src $(DOCKERLIB) bnfctensor:latest

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
	ghc --make $< $(LDOPTS) -o tensorkernel $(GHCOPTS)

tensor: Main.hs XPerm.hs $(HSMODULES) $(CANONICALIZE)
	ghc --make $< $(LDOPTS) -o tensor $(GHCOPTS)

test: Test.hs $(HSMODULES) $(CANONICALIZE)
	LD_PRELOAD="$(LIBDIR)/libcanonicalize.so" runhaskell RunTest.hs
	LD_PRELOAD="$(LIBDIR)/libcanonicalize.so" doctest $(HSMODULES)
	
docker: Main.hs $(HSMODULES)
	docker run $(DOCKER) make tensor

docker-test: Test.hs $(HSMODULES)
	docker run $(DOCKER) make test

docker-ghcid: $(CANONICALIZE)
	docker run $(DOCKER) ghcid "--command=ghci -fobject-code $(GHCOPTS)" 

docker-ghci: $(CANONICALIZE)
	docker run -v $(HOME)/.ghc/ghci_history:/root/.ghc/ghci_history $(DOCKER) ghci -lstdc++ -L../canonicalize/ -lcanonicalize -fobject-code

docker-ghci-debug:
	docker run -v $(HOME)/.ghc/ghci_history:/root/.ghc/ghci_history $(DOCKER) ghci -fexternal-interpreter -prof

clean:
	-rm -f tensor *.hi *.dyn_hi *.o *.dyn_o Frontend/*.hi Frontend/*.o Frontend/*.hs Frontend/*.x Frontend/*.y
	$(MAKE) -C ../canonicalize clean

docker-clean:
	docker run $(DOCKER) make clean

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

