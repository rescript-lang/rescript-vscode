#!/bin/zsh

./_build/install/default/bin/rescript-editor-support.exe -test definition test/src/Definition.res 2 8


cat test/src/Complete.res test/src/Complete.append >test/src/Complete.current	
./_build/install/default/bin/rescript-editor-support.exe -test complete test/src/Complete.res 1 6 test/src/Complete.current

