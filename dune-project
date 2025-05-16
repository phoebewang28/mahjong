(lang dune 3.17)

(using ctypes 0.3)

(name mahjong)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name mahjong)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html

(package
 (name raylib)
 (version 1.4.0)
 (description "OCaml bindings for raylib")
 (synopsis "OCaml bindings for raylib")
 (depends
  (dune
   (>= 3.17))
  dune-configurator
  (ctypes
   (>= 0.14))
  (integers
   (>= 0.5))
  (patch
   (>= 2.0))))

(package
 (name raygui)
 (version 1.4.0)
 (description "OCaml bindings for raygui")
 (synopsis "OCaml bindings for raygui")
 (depends
  (ocaml
   (>= 4.05))
  (dune
   (>= 3.17))
  dune-configurator
  (ctypes
   (>= 0.14))
  (raylib
   (>= 1.4.0))
  (patch
   (>= 2.0))))

(package
 (name raylib-gen)
 (allow_empty)
 (version 0.1.0)
 (description "Binding generation for raylib")
 (synopsis "Binding generation for raylib")
 (depends
  (dune
   (>= 3.17))
  yojson
  re))
