(lang dune 3.0)

(using dune_site 0.1)

(wrapped_executables false)

(package
  (name armdecode)
  (synopsis "ARM decode info extractor")
  (depends
    (libsail (= :version))))
