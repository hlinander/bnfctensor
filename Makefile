.PHONY: build-docker all ghci clean ghci-debug

all:
	docker run --rm -v $(PWD):/bnfctensor -w /bnfctensor/src bnfctensor:latest make

clean:
	(cd src && make clean)

build-docker:
	docker build -f docker/Dockerfile docker/ -t bnfctensor
	docker build -f docker/Dockerfile.debug docker/ -t bnfctensor:debug

ghcid:
	(cd src && make docker-ghcid)

ghci:
	(cd src && make docker-ghci)

ghci-debug:
	(cd src && make docker-ghci-debug)

run:
	docker run --rm -it -e "LD_LIBRARY_PATH=/bnfctensor/canonicalize" -v $(PWD):/bnfctensor -w /bnfctensor/src bnfctensor:latest ./tensor

term:
	docker run --cap-add=SYS_PTRACE --security-opt seccomp=unconfined --rm -it -v $(PWD):/bnfctensor -w /bnfctensor/src bnfctensor:latest /bin/bash

test:
	(cd src && make docker-test)
