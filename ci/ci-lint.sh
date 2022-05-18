#!/bin/bash
# Lint
# If run from the asclepias root directory, the hlint app is configured by 
# .hlint.yaml.
set -e

hlint  .

shellcheck scripts/*.sh
shellcheck ci/*.sh