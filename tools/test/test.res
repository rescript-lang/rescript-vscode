/***
Gerenate the json for some lib

Example for rescript-core

```sh
./src/Cli.bs.js doc /path/to/rescript-core/src/RescriptCore.res > test.json
````
*/
@module("fs") external readFileSync: string => string = "readFileSync"

let json = readFileSync("./test.json")->Js.Json.parseExn

RescriptTools.Docgen.decodeFromJson(json)->Js.log
