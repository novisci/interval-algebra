FROM registry.novisci.com/nsstat/statocker/haskell:8.10.4
RUN cabal update
COPY interval-algebra.cabal .
RUN cabal build --only-dependencies