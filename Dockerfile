FROM haskell:8.6.3
RUN cabal update
RUN cabal install combinat ansi-terminal QuickCheck generic-arbitrary
RUN apt update
RUN apt install -y apt libicu-dev libtinfo-dev libgmp-dev
RUN git clone https://github.com/haskell/haskell-ide-engine --recurse-submodules /hie
WORKDIR /hie
RUN make hie-8.6.3
RUN make build-doc-8.6.3
