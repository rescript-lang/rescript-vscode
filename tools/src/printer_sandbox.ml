(*
#use "prettier_printer.ml";;

    Interpret using  ocaml ./tools/src/printer_sandbox.ml


open DSL

let oak =
  Record
    [
      {
        name = "hello";
        value =
          Record
            [
              {name = "foo"; value = Ident "baaaaaaaaaaaaaaaaar"};
              {name = "member"; value = Ident "baazaaaaaaar"};
            ];
      };
      {
        name = "roxas";
        value =
          List
            [
              Ident "jjjjjjjjjjjjjjjjjjjjjjjjjjjjjjj";
              Ident "meeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee";
              Record
                [
                  {name = "foo"; value = Ident "baaaaaaaaaaaaaaaaar"};
                  {name = "member"; value = Ident "bazaaaaaaaaar"};
                ];
            ];
      };
      {name = "foo"; value = Ident "baaaaaaaaaaaaaaaaar"};
    ]

let _ =
  CodePrinter.genOak oak {CodePrinter.emptyContext with max_line_length = 20}
  |> CodePrinter.dump |> Format.printf "%s\n"
*)