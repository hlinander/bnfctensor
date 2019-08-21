FROM haskell:8.6.3
RUN apt-get update && apt install -y \
	apt \
	libicu-dev \
	libtinfo-dev \
	libgmp-dev \
	libreadline-dev \
	bnfc \
	alex \
	happy \
	libzmq3-dev \
	ipython \ 
	python-jupyter-client \
	gdb
RUN cabal update && cabal install \
	combinat \
	ansi-terminal \
	QuickCheck \
	generic-arbitrary \
	readline \
	split \
	ipython-kernel-0.9.1.0 \
	lens \
	uniplate \
	tree-view
RUN cabal install ghcid inline-c inline-c-cpp
RUN curl https://sh.rustup.rs -sSf | sh -s -- -y
ENV PATH=/root/.cargo/bin:$PATH

# Fix for GHC packages when starting container as non-root user
RUN chmod a+rwx /root
ENV HOME=/root
#RUN git clone https://github.com/haskell/haskell-ide-engine --recurse-submodules /hie
#WORKDIR /hie
#RUN stack ./install.hs hie-8.6.5
#RUN stack ./install.hs build-data
