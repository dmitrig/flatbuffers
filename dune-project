(lang dune 3.4)

(name flatbuffers)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license Apache-2.0)

(documentation https://url/to/documentation)

(package
 (name flatbuffers)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  (ocaml (>= 4.13))
   bigstringaf
  (cppo (and :build (>= 1.1.0)))
  (memtrace :with-test)
  (benchmark :with-test)
  (alcotest (and :with-test (>= 1.7))))
 (tags
  (topics "to describe" your project)))

; to make running ocamldebug easier
(wrapped_executables false)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
