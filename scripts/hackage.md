# Uploading to hackage

* `cabal test`
* `cabal sdist`
* upload the resulting `.tar.gz` as a release candidate using `cabal upload`
* check coverage of the documentation using `cabal haddock`
* upload the documentation using `./scripts/hackage-docs.sh`
* review the release candidate on hackage. If satisfied, publish with `cabal upload --publish` or using the admin interface on hackage.
