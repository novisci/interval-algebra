ARG GHC
FROM registry.novisci.com/nsstat/statocker/haskell:${GHC}
WORKDIR /project
RUN cabal update
COPY interval-algebra.cabal .
RUN cabal build --only-dependencies --enable-tests