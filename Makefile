.PHONY: build-docker all ghci clean ghci-debug

all:
	docker run --rm -v $(PWD)/src:/bnfctensor -w /bnfctensor bnfctensor:latest make

clean:
	(cd src && make clean)

build-docker:
	docker build -f docker/Dockerfile docker/ -t bnfctensor
	docker build -f docker/Dockerfile.debug docker/ -t bnfctensor:debug

ghci:
	(cd src && make docker-ghci)

ghci-debug:
	(cd src && make docker-ghci-debug)

run:
	docker run --rm -it -v $(PWD):/bnfctensor -w /bnfctensor/src bnfctensor:latest ./tensor

test:
	(cd src && make docker-test)
