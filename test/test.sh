#!/bin/zsh

../_build/install/default/bin/rescript-editor-support.exe test src/Definition.res >src/Definition.exp

../_build/install/default/bin/rescript-editor-support.exe test src/Complete.res >src/Complete.exp
