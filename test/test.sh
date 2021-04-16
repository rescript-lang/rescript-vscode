#!/bin/zsh

function exp {
  echo "$(dirname $1)/expected/$(basename $1).txt"
}

taskCount=0
function maybeWait {
  let taskCount+=1
  # spawn in batch of 20 processes
  [[ $((taskCount % 20)) = 0 ]] && wait
}

for file in src/**/*.res; do
  ../_build/install/default/bin/rescript-editor-support.exe test $file &> $(exp $file) & maybeWait
done
