file=BuildSystem
ocamlc -dsource -ppx _build/default/src/ppx2/Ppx_Unmonads.exe -pp "refmt --parse re --print binary" -I src -impl src/rescript-editor-support/$file.re &> ./temp.txt
# refmt --parse ml --print re ./temp.txt &> src/rescript-editor-support/$file.re
