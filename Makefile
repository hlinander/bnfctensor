.PHONY: all clean

all: tensor

tensor: Tensor.hs Frontend/LexTensor.hs Frontend/ParTensor.hs
	ghc --make $< -o tensor

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
