#!/bin/sh

echo "$CI_REGISTRY_PASSWORD" |
  docker login \
    --username "$CI_REGISTRY_USER" \
    --password-stdin "$CI_REGISTRY"

docker build \
  --tag "$CI_REGISTRY_IMAGE"/"$PKG"-build:"$VERSION" \
  --tag "$CI_REGISTRY_IMAGE"/"$PKG"-build:latest \
  --build-arg GHC="$GHC" \
  --file ci/Dockerfile  . 

docker push "$CI_REGISTRY_IMAGE"/"$PKG"-build:"$VERSION"
docker push "$CI_REGISTRY_IMAGE"/"$PKG"-build:latest

docker logout
