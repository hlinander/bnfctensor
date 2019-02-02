.PHONY: build-docker all ghci clean

all:
	docker run --rm -v $(PWD)/src:/bnfctensor -w /bnfctensor bnfctensor:latest make

clean:
	(cd src && make clean)

build-docker:
	docker build . -t bnfctensor

ghci:
	(cd src && make docker-ghci)

run:
	docker run --rm -it -v $(PWD)/src:/bnfctensor -w /bnfctensor bnfctensor:latest /bnfctensor/tensor
