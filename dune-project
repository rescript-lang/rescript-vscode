(lang dune 3.0)

(generate_opam_files true)

(authors "Cristiano Calcagno")

(maintainers "Cristiano Calcagno")

(homepage "https://github.com/rescript-lang/rescript-vscode")

(bug_reports "https://github.com/rescript-lang/rescript-vscode/issues")

(package
 (name analysis)
 (synopsis "ReScript Analysis")
 (depends
  (ocaml
   (>= 4.10))
  (ocamlformat
   (= 0.22.4))
  (cppo
   (= 1.6.9))
  (reanalyze
   (= 2.23.0))))
