#!/usr/bin/env bash

set -e

for file in ./examples/nrmd/*.dhall ; do
  filename=$(basename -- "$file");
  extension="${filename##*.}";
  rootname="${filename%.*}"
  echo "merged file $rootname.yaml with defaults:"
  echo "$(yq -s '.[0] * .[1]' ./resources/defaults/nrmd.json ./examples/nrmd/${rootname}.yaml)"
  echo "diffing with file: $file"
  dhall diff " $file " " $(yq -s '.[0] * .[1]' ./resources/defaults/nrmd.json ./examples/nrmd/${rootname}.yaml | json-to-dhall ' (./hsnrm/hsnrm/dhall/types/nrmd.dhall).Cfg ' --records-loose) "
  cat ./examples/nrmd/${rootname}.yaml | yq '' > ./examples/nrmd/${rootname}.json
done

for file in ./examples/manifests/*.dhall ; do
  filename=$(basename -- "$file");
  extension="${filename##*.}";
  rootname="${filename%.*}"
  echo "merged file $rootname.yaml with defaults:"
  echo "$(yq -s '.[0] * .[1]' ./resources/defaults/manifest.json ./examples/manifests/${rootname}.yaml)"
  echo "diffing with file: $file"
  dhall diff " $file " " $(yq -s '.[0] * .[1]' ./resources/defaults/manifest.json ./examples/manifests/${rootname}.yaml | json-to-dhall ' (./hsnrm/hsnrm/dhall/types/manifest.dhall).Manifest ' --records-loose) "
  cat ./examples/manifests/${rootname}.yaml | yq '' > ./examples/manifests/${rootname}.json
done

