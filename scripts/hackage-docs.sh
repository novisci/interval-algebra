#!/bin/sh
set -e

dir=$(mktemp -d dist-docs.XXXXXX)
trap 'rm -r "$dir"' EXIT

# assumes cabal 2.4 or later
# NOTE: docs build fails with these options when compiler is newer
cabal haddock --with-compiler ghc-8.10.7 --builddir="$dir" --haddock-for-hackage --enable-doc


# cabal upload -d --publish $dir/*-docs.tar.gz
cabal upload -d "$dir"/*-docs.tar.gz
