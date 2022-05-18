#!/bin/bash
#
# To run locally:
# PKG=interval-algebra VERSION=1.4.0 ./ci/ci-coverage.sh
#
# In the CI, 
# the PKG and VERSION variables are defined in the build-vars stage.
# 
# NOTE: to work in the CI context the appropriate paths need to have been saved
#       as artifacts in previous jobs.

echo " "
echo "-------Gathering coverage for $PKG-$VERSION-------"
echo " "

# Find all .tix files and put in bash array
# The following is lifted from
# https://stackoverflow.com/questions/23356779/how-can-i-store-the-find-command-results-as-an-array-in-bash
tix=()
while IFS=  read -r -d $'\0'; do
    tix+=("$REPLY")
done < <(find . -name "$PKG"-"$VERSION".tix -print0)

# remove leading point and slash,
# as they seem to give trouble to hpc 
tixFiles=()
for tx in "${tix[@]}" 
do 
  x=${tx#.}
  tixFiles+=("${x#/}")
done

echo "${tixFiles[@]}"

# "Sum multiple .tix files in a single .tix file"
#
# FIXME: 
# The tix files includes reports from other packages (hspec and QuickCheck).
# The hpc options seems to suggest that one can include/exclude reports
# from sum by "MODULE and/or PACKAGE". 
# For the life of me, I could not figure how to include or exclude by package.
# Hence, I decided to simply explicitly include interval algebra modules.
# This is of course fragile since the modules are hard-coded below.
# Surely, there's a better way to do this.

hpc sum "${tixFiles[@]}" \
  --output=coverage.tix \
  --include=IntervalAlgebra.Core \
  --include=IntervalAlgebra.Arbitrary \
  --include=IntervalAlgebra.Axioms \
  --include=IntervalAlgebra.IntervalUtilites \
  --include=IntervalAlgebra.IntervalDiagram \
  --include=IntervalAlgebra.PairedInterval \
  --include=IntervalAlgebra.RelationProperties \

# FIXME: hardcoded "x86_64-linux" in --srcdir
hpc report coverage.tix \
    --srcdir=dist-newstyle/build/x86_64-linux/ghc-8.10.7/"$PKG"-"$VERSION"\
    --hpcdir=hpc/vanilla/mix/"$PKG"-"$VERSION"
