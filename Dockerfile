FROM haskell:8.6.3
RUN cabal update
RUN cabal install combinat
