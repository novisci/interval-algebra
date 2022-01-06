#!/bin/sh

# Create a local for artifacts
ARTIFACT_DIR=coverage
mkdir -p $ARTIFACT_DIR

# NOTE: to work in the CI context the appropriate paths need to have been saved
#       as artifacts in previous jobs.
tix=$(find ./dist-newstyle -name "$PKG"-"$VERSION".tix)
mix=$(find ./dist-newstyle -name vanilla -print -quit)/mix/"$PKG"-"$VERSION"

# Copy coverage report to artifacts directory
# TODO: it's not clear how to convert hpc output to a complete cobertura report as XML
# (here's an example report: https://gist.github.com/apetro/fcfffb8c4cdab2c1061d)
# hpc report can produce *summary* information, but for gitlab's
# [Test information visualization](https://docs.gitlab.com/ee/user/project/merge_requests/test_coverage_visualization.html)
# to work, we need more than just the summary as XML.
# hpc report "$tix" \
#   --xml-output \
#   --per-module \
#   --hpcdir="$mix" > coverage.xml

# Report the overall coverage
hpc report "$tix" --hpcdir="$mix"
