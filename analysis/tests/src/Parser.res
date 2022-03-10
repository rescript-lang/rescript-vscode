module M = {
  module C = Component
}

let _c = <Component />

let _mc = <M.C />

let _d = <div />

let _d2 =
  <div>
    {React.string("abc")}
    <div> {React.string("abc")} </div>
    {React.string("abc")}
    {React.string("abc")}
  </div>

type pair<'x, 'y> = ('x, 'y)

type looooooooooooooooooooooooooooooooooooooong_int = int

type looooooooooooooooooooooooooooooooooooooong_string = string

type pairIntString = list<
  pair<
    looooooooooooooooooooooooooooooooooooooong_int,
    looooooooooooooooooooooooooooooooooooooong_string,
  >,
>

let _ = 3 < 4 || 3 > 4

// ^par 
