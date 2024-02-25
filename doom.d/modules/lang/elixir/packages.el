;; -*- no-byte-compile: t; -*-
;;; lang/elixir/packages.el

;; +elixir.el
(package! heex-ts-mode :pin "90142df2929956536dc1eaae3bb5ca04dc4232ab")
(package! elixir-ts-mode :pin "6db05baed9a34d01edf0bfdd804d951dedc6dccb")
(package! exunit :pin "e008c89e01e5680473278c7e7bab42842e294e4d")
(when (and (modulep! :checkers syntax)
           (not (modulep! :checkers syntax +flymake)))
  (package! flycheck-credo :pin "e285bd042a535d0f13e0b4c5226df404cdda4033"))
