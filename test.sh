#!/bin/zsh

./_build/install/default/bin/rescript-editor-support.exe -test definition test/src/Definition.res 2 8

./_build/install/default/bin/rescript-editor-support.exe test test/src/Complete.res
