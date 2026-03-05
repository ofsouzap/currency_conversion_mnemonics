(lang dune 3.21)

(name currency_conversion_mnemonics)

(generate_opam_files true)

(source
 (github ofsouzap/currency_conversion_mnemonics))

(authors "Ofsouzap <ofsouzap@gmail.com>")

(maintainers "Ofsouzap <ofsouzap@gmail.com>")

(license MIT)

(package
 (name currency_conversion_mnemonics)
 (synopsis "Mnemonics for currency conversions")
 (allow_empty)
 (depends
  ocaml
  core
  zarith
  ppx_expect
  ppx_inline_test
  (quickterface
   (= 0.1.0))))
