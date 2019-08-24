.PHONY: build-docker all ghci clean ghci-debug

UID = $(shell id -u)
GID = $(shell id -g)
DOCKERLIB = -e "LD_LIBRARY_PATH=/bnfctensor/canonicalize" 
DOCKERDEBUG = --cap-add=SYS_PTRACE --security-opt seccomp=unconfined
DOCKER = --rm -it -u $(UID):$(GID) -v $(PWD):/bnfctensor -w /bnfctensor/src $(DOCKERLIB) bnfctensor:latest

JUPYTER_KERNEL_PATH = $(HOME)/.local/share/jupyter/kernels/tensorkernel

all:
	docker run $(DOCKER) make

clean:
	(cd src && make docker-clean)

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
	docker run $(DOCKER) ./tensor

term:
	docker run $(DOCKERDEBUG) $(DOCKER) /bin/bash

root-term:
	docker run --rm -it bnfctensor:latest /bin/bash

test:
	(cd src && make docker-test)

install-jupyter:
	mkdir -p $(JUPYTER_KERNEL_PATH)
	BNFCTENSOR=$(PWD) envsubst < jupyter/kernel.env.json > $(JUPYTER_KERNEL_PATH)/kernel.json
	cp jupyter/kernel.js $(JUPYTER_KERNEL_PATH)/kernel.js
	cp jupyter/kernel.css $(JUPYTER_KERNEL_PATH)/kernel.css

jupyter: install-jupyter
	jupyter-notebook --browser=firefox
