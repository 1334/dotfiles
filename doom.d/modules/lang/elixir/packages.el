;; -*- no-byte-compile: t; -*-
;;; lang/elixir/packages.el

;; +elixir.el
(package! elixir-ts-mode)
(package! heex-ts-mode)
(package! exunit :pin "ee06b14b61beaf59d847802b83b027ac991f3496")
(when (and (modulep! :checkers syntax)
           (not (modulep! :checkers syntax +flymake)))
  (package! flycheck-credo :pin "e285bd042a535d0f13e0b4c5226df404cdda4033"))
