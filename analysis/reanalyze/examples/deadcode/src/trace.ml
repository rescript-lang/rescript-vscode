type ('a, 'b) fmt = ('a, Format.formatter, unit, 'b) format4
type 'a printf = ('a, unit) fmt -> 'a
type pf = {pf: 'a. 'a printf}

let infok _mod_name _fun_name k = k {pf= Format.eprintf}
