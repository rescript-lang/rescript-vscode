#!/bin/zsh

./_build/install/default/bin/rescript-editor-support.exe definition test/src/Definition.res 2 8


cat test/src/Complete.res test/src/Complete.append >test/src/Complete.current	
./_build/install/default/bin/rescript-editor-support.exe complete test/src/Complete.res 1 6 test/src/Complete.current

