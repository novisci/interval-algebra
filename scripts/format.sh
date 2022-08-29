#!/bin/sh
# Formats haskell source code files in place using brittany source code formatter.

# Check and see if `brittany` is installed so as to give users a useful message
# if they don't know what brittany is or how to install it
if ! [ -x "$(command -v brittany)" ]; then
  echo 'Error: cannot find '\''brittany'\'' application' >&2
  echo 'Use the command '\''cabal install brittany'\'' to install the application'  >&2
  exit 1
fi
find . -name '*.hs'                           \
  -not -path './dist-newstyle/*'              \
  -not -path './docs-site/*'                  \
  -not -path './docs/*'                       \
  -not -path './tutorial/TutorialMain.hs'     \
  -exec stylish-haskell --config ci/ci-stylish-haskell.yaml --inplace {} ';'
