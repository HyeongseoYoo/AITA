(lang dune 3.7)

(name aita-server)

(generate_opam_files true)

(source
 (github HyeongseoYoo/AITA))

(authors
 "Hyeongseo Yoo <hsyoo@ropas.snu.ac.kr>"
 "Jay Lee <jhlee@ropas.snu.ac.kr>")

(maintainers
 "Hyeongseo Yoo <hsyoo@ropas.snu.ac.kr>"
 "Jay Lee <jhlee@ropas.snu.ac.kr>")

(package
 (name aita-server)
 (synopsis "AI TA server")
 (description "AI Teaching Assistant server for the AITA project.")
 (depends
  ocaml
  (ocaml-base-compiler
   (= 4.14.0))
  dune
  dream
  cohttp-lwt-unix
  yojson
  ppx_yojson_conv)
 (tags
  ("static analysis" LLM)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
