(lang dune 3.17)

(name lambda)

(generate_opam_files false)

(source
 (github 0xmycf/lambda))

(using menhir 3.0)

(authors "0xmycf")
(maintainers "0xmycf")
(license MIT)
(documentation https://github.com/0xmycf/lambda)

(package
 (name lambda)
 (synopsis "A simple ocaml template project")
 (description "This project is a simple ocaml template project.")
 (depends ocaml dune))

