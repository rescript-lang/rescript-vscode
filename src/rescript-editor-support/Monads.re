module Option = {
  type t('a) = option('a);
  let bind = (value, ~f as use) =>
    switch (value) {
    | Some(x) => use(x)
    | None => None
    };
  let consume = (value, ~f as use) =>
    switch (value) {
    | Some(x) => use(x)
    | None => ()
    };
};

module O = Option;

module Result = {
  let map /*: t 'a 'b => f::('a => 'c) => t 'c 'b*/ = (value, ~f as use) =>
    switch (value) {
    | Ok(x) => Ok(use(x))
    | Error(e) => Error(e)
    };

  let bind: (result('a, 'b), ~f: 'a => result('c, 'b)) => result('c, 'b) =
    (value, ~f as use) =>
      switch (value) {
      | Ok(x) => use(x)
      | Error(e) => Error(e)
      };
  let consume: (result('a, 'b), ~f: 'a => unit) => unit =
    (value, ~f as use) =>
      switch (value) {
      | Ok(x) => use(x)
      | Error(_) => ()
      };
};
