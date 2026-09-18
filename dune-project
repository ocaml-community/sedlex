(lang dune 3.18)
(using menhir 2.1)
(version 3.7)
(name sedlex)
(source (github ocaml-community/sedlex))
(license MIT)
(authors "Alain Frisch <alain.frisch@lexifi.com>"
         "https://github.com/ocaml-community/sedlex/graphs/contributors")
(maintainers "Alain Frisch <alain.frisch@lexifi.com>")
(homepage "https://github.com/ocaml-community/sedlex")
(maintenance_intent "(latest)")
(documentation "https://ocaml-community.github.io/sedlex/index.html")

(generate_opam_files true)
(executables_implicit_empty_intf true)

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
   (ppx_expect :with-test)
   (qcheck-core :with-test)
   (menhir :with-test)))
