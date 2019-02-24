.PHONY: all clean docker-ghci docker docker-ghci-debug

all: tensor tensorkernel

tensorkernel: Kernel.hs Eval.hs Core.hs Tensor.hs Check.hs Frontend/LexTensor.hs Frontend/ParTensor.hs RenderCalc.hs
	ghc --make $< -o tensorkernel

tensor: Main.hs Eval.hs Core.hs Tensor.hs Check.hs Frontend/LexTensor.hs Frontend/ParTensor.hs RenderCalc.hs
	ghc --make $< -o tensor

docker: Main.hs Core.hs Tensor.hs Check.hs Frontend/LexTensor.hs Frontend/ParTensor.hs RenderCalc.hs
	docker run --rm -v $(PWD):/bnfctensor -w /bnfctensor bnfctensor:latest make tensor

docker-ghci:
	docker run --rm -it -v $(PWD):/bnfctensor -v $(HOME)/.ghc/ghci_history:/root/.ghc/ghci_history -w /bnfctensor bnfctensor:latest

docker-ghci-debug:
	docker run --rm -it -v $(PWD):/bnfctensor -v $(HOME)/.ghc/ghci_history:/root/.ghc/ghci_history -w /bnfctensor bnfctensor:debug2 ghci -fexternal-interpreter -prof

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
