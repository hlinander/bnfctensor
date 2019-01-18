FROM haskell:8.4.3
RUN cabal update
RUN cabal install combinat ansi-terminal
