#!/bin/zsh

function exp {
  echo "$(dirname $1)/expected/$(basename $1).txt"
}

./node_modules/.bin/rescript
for file in src/**/*.res; do
  ../lib/rescript-editor-support.exe test $file &> $(exp $file)
done
