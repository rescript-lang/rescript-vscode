(lang dune 2.3)

(generate_opam_files true)

(authors "ReScript Team")

(maintainers "ReScript Team")

(homepage "https://github.com/rescript-lang/rescript-vscode")

(bug_reports "https://github.com/rescript-lang/rescript-vscode/issues")

(package
 (name analysis)
 (synopsis "ReScript Analysis")
 (depends
  (ocaml
   (>= 4.10))
  (cppo
   (= 1.6.9))
  dune))

(package
 (name tools)
 (synopsis "ReScript Tools")
 (depends
  (ocaml
   (>= 4.10))
  (cppo
   (= 1.6.9))
  (cmdliner
    (>= 1.2))
  analysis
  dune))
