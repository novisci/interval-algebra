#!/bin/sh
# Checks that the source code is formatted using the stylish-haskell
# formatter (https://hackage.haskell.org/package/stylish-haskell)
set -e 

stylish-haskell --version

# Check for changes and return exit code 1 if any are necessary. Note that we
# can't use `-exec` here since the return code of the exec'd command isn't
# propagated to the `find` process. See e.g. the following for details:
# https://apple.stackexchange.com/a/49156/454179
find . -name "*.hs"                        \
  -not -path './dist-newstyle/*'           \
  -not -path './docs-site/*'               \
  -not -path './docs/*'                    \
  -not -path './tutorial/TutorialMain.hs'  \
  -print0  |
  xargs -0 stylish-haskell --config ci/ci-stylish-haskell.yaml
