(lang dune 3.2)
(using dune_site 0.1)
(name frama-c-ktsn)
(maintainers "Tomas Brablec")

(package
  (name frama-c-ktsn)
  (synopsis "Frama-C plugin for shape analysis using separation logic")
  (depends
    (ocaml (>= 5.1.0))
    (frama-c (= 31.0))
    astral
    sha
    uuidm
    (ppx_inline_test :build)
  )
)
(generate_opam_files)
