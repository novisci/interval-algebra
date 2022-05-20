#!/bin/bash
#
# Runs the project's test suite. If the DRAFT flag is on then only the axioms
# are tested (which take less time). Turn the DRAFT flag on by adding '-draft' 
# (no quotes) to a commit message.

echo "$DRAFT"

if [[ $DRAFT ]]
 then 
cabal test axioms \
  --test-show-details=always 
  # \
  # --enable-coverage 
else
cabal test \
  -j \
  --test-show-details=always 
  # \
  # --enable-coverage
fi

# Run doctests
cabal repl --with-ghc=doctest
