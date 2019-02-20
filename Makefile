.PHONY: build-docker all ghci clean ghci-debug

all:
	docker run --rm -v $(PWD)/src:/bnfctensor -w /bnfctensor bnfctensor:latest make

clean:
	(cd src && make clean)

build-docker:
	docker build . -t bnfctensor

ghci:
	(cd src && make docker-ghci)

ghci-debug:
	(cd src && make docker-ghci-debug)

run:
	docker run --rm -it -v $(PWD)/src:/bnfctensor -w /bnfctensor bnfctensor:latest /bnfctensor/tensor
