type t = {n: array<string>}

let t = {n: []}

type t2 = {n2: t}

let t2 = {n2: t}

// ^com t.n->m

// ^com t2.n2.n->m
