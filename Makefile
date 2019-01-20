.PHONY: build-docker all ghci clean

all:
	(cd src && make docker)

clean:
	(cd src && make clean)

build-docker:
	docker build . -t bnfctensor

ghci:
	(cd src && make docker-ghci)

	
