(lang dune 3.7)

;; from 2.3 to 3.7, for earlybird debugger

(generate_opam_files true)

(authors "ReScript Team")

(maintainers "ReScript Team")

(homepage "https://github.com/rescript-lang/rescript-vscode")

(bug_reports "https://github.com/rescript-lang/rescript-vscode/issues")

;; for earlybird debugger

(map_workspace_root false)

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
  analysis
  dune))
