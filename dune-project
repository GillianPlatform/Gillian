(lang dune 3.16)

(name gillian)

(using menhir 2.0)

(using cinaps 1.0)

(using dune_site 0.1)

(generate_opam_files)

(license "BSD-3-Clause")

(authors "The Gillian Team")

(maintainers "The Gillian Team")

(homepage "https://github.com/GillianPlatform/Gillian")

(bug_reports "https://github.com/GillianPlatform/Gillian/issues")

(source
 (github GillianPlatform/Gillian))

(package
 (name gillian)
 (synopsis
  "A parametric compositional symbolic execution for Correctness and Incorrectness")
 (depends
  (ocaml
   (>= 5.1.0))
  (alcotest
   (= 1.0.1))
  (cmdliner
   (>= 1.1.1))
  sedap
  (fmt
   (>= 0.8.8))
  (linol-lwt
   (>= 0.8))
  (memtrace
   (= 0.2.3))
  (menhir
   (= "20231231"))
  ppx_deriving_yojson
  (ppxlib
   (>= 0.18.0))
  sexplib
  simple_smt
  (sqlite3
   (= 5.0.2))
  (visitors
   (>= 2.3))
  (yojson
   (>= 1.7.0))
  (zarith
   (>= 1.12))
  (odoc :with-doc)))

(package
 (name gillian-js)
 (sites
  (share runtime))
 (depends
  dune-site
  flow_parser
  fpath
  (gillian
   (= :version)))
 (synopsis "Gillian instantiation for JavaScript"))

(package
 (name gillian-c)
 (sites
  (share runtime)
  (share c_includes))
 (synopsis "Gillian instantiation for C")
 (depends
  (gillian
   (= :version))
  compcert
  dune-site
  printbox-text))

(package
 (name gillian-c2)
 (sites
  (share runtime))
 (synopsis "Gillian instantiation for C based on CBMC as front-end")
 (depends
  (gillian
   (= :version))
  dune-site
  printbox-text))

(package
 (name wisl)
 (sites
  (share runtime))
 (depends
  dune-site
  (gillian
   (= :version)))
 (synopsis
  "Gillian instantiation for Wisl, a small toy language for teaching and experimentation"))

(package
 (name transformers)
 (sites
  (share runtime))
 (synopsis "Gillian instantiation using state model transformers, à la Iris")
 (depends
  (gillian
   (= :version))
  (gillian-c
   (= :version))
  (gillian-js
   (= :version))
  dune-site
  printbox-text))
