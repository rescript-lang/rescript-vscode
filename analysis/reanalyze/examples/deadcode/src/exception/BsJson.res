open JsonCombinators

@raise(JsonCombinators.DecodeError)
let testBsJson2 = x => x->Json.decode(Json.Decode.string)
