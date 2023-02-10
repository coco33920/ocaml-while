(lang dune 3.6)

(name ocaml_while)

(generate_opam_files true)

(source
 (github coco33920/ocaml-while))

(authors "Charlotte Thomas")

(maintainers "Charlotte Thomas")

(license GPL-3.0-or-later)


(package
 (name ocaml_while)
 (synopsis "An implementation of the while programming language")
 (description "OCaml Implementation of the while programming language as seen in Calculability, 4th semester, 2nd year, Bachelor of Science, University of Rennes")
 (depends ocaml dune))