#!/bin/zsh

function exp {
  echo "$(dirname $1)/expected/$(basename $1).txt"
}

./node_modules/.bin/rescript build
for file in src/**/*.res; do
  ../_build/install/default/bin/rescript-editor-support.exe test $file &> $(exp $file)
done
