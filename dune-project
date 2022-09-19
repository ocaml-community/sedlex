(lang dune 2.8)
(version 3.0)
(name sedlex)
(source (github ocaml-community/sedlex))
(license MIT)
(authors "Alain Frisch <alain.frisch@lexifi.com>"
         "https://github.com/ocaml-community/sedlex/graphs/contributors")
(maintainers "Alain Frisch <alain.frisch@lexifi.com>")
(homepage "https://github.com/ocaml-community/sedlex")

(generate_opam_files true)

(package
  (name sedlex)
  (synopsis "An OCaml lexer generator for Unicode")
  (description "sedlex is a lexer generator for OCaml. It is similar to ocamllex, but supports
Unicode. Unlike ocamllex, sedlex allows lexer specifications within regular
OCaml source files. Lexing specific constructs are provided via a ppx syntax
extension.")
 (depends
   (ocaml (>= 4.08))
   dune
   (ppxlib (>= 0.26.0))
   gen
   uchar))
