FROM haskell:8.6.3
RUN apt-get update && apt install -y apt libicu-dev libtinfo-dev libgmp-dev libreadline-dev
RUN cabal update && cabal install combinat ansi-terminal QuickCheck generic-arbitrary readline
RUN git clone https://github.com/haskell/haskell-ide-engine --recurse-submodules /hie
WORKDIR /hie
RUN make hie-8.6.3
RUN make build-doc-8.6.3
RUN apt install bnfc alex happy
