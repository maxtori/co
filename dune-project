(lang dune 3.21)
(generate_opam_files true)
(name co)
(source (github maxtori/co))
(license MIT)
(authors "maxtori")
(maintainers "maxtori")
(homepage https://github.com/maxtori/co)
(bug_reports https://github.com/maxtori/co/issues)
(documentation https://github.com/maxtori/co)

(package
 (name co)
 (synopsis "co")
 (depends
  ez_api ezjs_min ezjs_idb ezjs_fetch vue-ppx
  (ppx_deriving_encoding :build)
  (ppx_deriving_jsoo :build)
  (ppx_deriving_arg :build)))
