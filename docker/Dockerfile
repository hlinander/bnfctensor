FROM haskell:8.6.3
RUN apt-get update && apt install -y apt libicu-dev libtinfo-dev libgmp-dev libreadline-dev
RUN cabal update && cabal install combinat ansi-terminal QuickCheck generic-arbitrary readline
RUN apt install bnfc alex happy
RUN cabal install split
RUN apt-get install -y libzmq3-dev
RUN cabal install ipython-kernel
RUN apt-get install -y ipython python-jupyter-client
RUN cabal install lens
RUN cabal install uniplate
RUN cabal install tree-view
RUN cabal install inline-c
RUN cabal install inline-c-cpp
RUN apt-get update && apt-get install -y gdb
#RUN apt-get install -y cargo
RUN cabal install ghcid
#RUN git clone https://github.com/haskell/haskell-ide-engine --recurse-submodules /hie
#WORKDIR /hie
#RUN stack ./install.hs hie-8.6.5
#RUN stack ./install.hs build-data
# RUN make hie-8.6.3
# RUN make build-doc-8.6.3
RUN curl https://sh.rustup.rs -sSf | sh -s -- -y
ENV PATH=/root/.cargo/bin:$PATH
