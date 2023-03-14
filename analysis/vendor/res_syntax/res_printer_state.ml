let customLayoutThreshold = 2

type t = {
  customLayout: int;
  mutable uncurried_config: Res_uncurried.config;
  customInfixOperators: (string, string) Hashtbl.t;
}

let init =
  {
    customLayout = 0;
    uncurried_config = Res_uncurried.init;
    customInfixOperators = Hashtbl.create 0;
  }

let nextCustomLayout t = {t with customLayout = t.customLayout + 1}

let shouldBreakCallback t = t.customLayout > customLayoutThreshold
