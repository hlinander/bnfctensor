.PHONY: all clean

all: tensor

tensor: Main.hs Core.hs Tensor.hs Check.hs Frontend/LexTensor.hs Frontend/ParTensor.hs
	ghc --make $< -o tensor

docker: Main.hs Core.hs Tensor.hs Check.hs Frontend/LexTensor.hs Frontend/ParTensor.hs
	docker run --rm -v $(PWD):/bnfctensor -w /bnfctensor bnfctensor:latest make tensor

docker-ghci:
	docker run --rm -it -v $(PWD):/bnfctensor -w /bnfctensor bnfctensor:latest

clean:
	-rm -f Frontend/*.hi Frontend/*.o

LEXER = LexTensor.hs
PARSER = ParTensor.hs

VPATH = Frontend

Frontend/%.hs: %.y
	happy -gca $<

Frontend/%.hs: %.x
	alex -g $<

Frontend/ParTensor.y Frontend/LexTensor.x: Tensor.cf
	bnfc -p Frontend --ghc $<
