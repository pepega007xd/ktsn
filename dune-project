(lang dune 3.2)
(using dune_site 0.1)
(name frama-c-seal)
(maintainers "Tomas Brablec")

(package
  (name frama-c-seal)
  (synopsis "Frama-C plugin for shape analysis using separation logic")
  (depends
    (ocaml (>= 5.1.0))
    (frama-c (= 31.0))
    astral
    sha
    uuidm
    bitwuzla-cxx ; this will enable bitwuzla solver in astral
    (ppx_inline_test :build)
  )
)
(generate_opam_files)
