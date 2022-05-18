# This docker image is used by the project's CI
# to build the package's dependencies when the .cabal file changes.
# In this way, every CI pipelines deosn't need to build all dependencies
# from scratch,
# thus reducing CI times.
ARG GHC
FROM registry.novisci.com/nsstat/nsbuild/haskell:${GHC}
WORKDIR /project
RUN cabal update
COPY interval-algebra.cabal .
COPY cabal.project .
RUN cabal build --only-dependencies --enable-tests