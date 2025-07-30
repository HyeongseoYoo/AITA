(lang dune 3.7)

(name aita-server)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name aita-server)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
   ocaml
   dream
   mopsa
   cohttp-lwt-unix
   )
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
