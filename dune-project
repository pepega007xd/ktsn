(lang dune 3.2)
(using dune_site 0.1)
(name frama-c-ktsn)
(maintainers "Tomas Brablec")

(package
  (name frama-c-ktsn)
  (synopsis "Frama-C plugin for shape analysis using separation logic")
  (depends
    (frama-c (>= 30.0))
    ppx_inline_test
    (qcheck :with-test)
    (alcotest :with-test)
  )
)
(generate_opam_files)
