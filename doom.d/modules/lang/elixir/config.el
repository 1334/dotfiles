;;; lang/elixir/config.el -*- lexical-binding: t; -*-

;; DEPRECATED: Remove when projectile is replaced with project.el
(after! projectile
  (add-to-list 'projectile-project-root-files "mix.exs"))

(after! lsp-mode
  (add-to-list 'lsp-language-id-configuration '( "\\.heex\\'" . heex-ts-mode)))

;;
;;; Packages

(use-package! elixir-ts-mode
  :defer t
  :init
  ;; Disable default smartparens config. There are too many pairs; we only want
  ;; a subset of them (defined below).
  (provide 'smartparens-elixir)
  :config
  (set-ligatures! 'elixir-ts-mode
                  ;; Functional
                  :def "def"
                  :lambda "fn"
                  ;; :src_block "do"
                  ;; :src_block_end "end"
                  ;; Flow
                  :not "!"
                  :in "in" :not-in "not in"
                  :and "and" :or "or"
                  :for "for"
                  :return "return" :yield "use")

  ;; ...and only complete the basics
  (sp-with-modes 'elixir-ts-mode
    (sp-local-pair "do" "end"
                   :when '(("RET" "<evil-ret>"))
                   :unless '(sp-in-comment-p sp-in-string-p)
                   :post-handlers '("||\n[i]"))
    (sp-local-pair "do " " end" :unless '(sp-in-comment-p sp-in-string-p))
    (sp-local-pair "fn " " end" :unless '(sp-in-comment-p sp-in-string-p)))


  (when (modulep! +lsp)
    (add-hook 'elixir-ts-mode-local-vars-hook #'lsp! 'append)
    (after! lsp-mode
      (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]_build\\'")))

  (when (modulep! +tree-sitter)
    (add-hook 'elixir-ts-mode-local-vars-hook #'tree-sitter! 'append)
    (after! tree-sitter
      (add-to-list 'tree-sitter-major-mode-language-alist '(elixir-ts-mode . elixir))))
  )

(use-package heex-ts-mode
  :hook (heex-ts-mode . lsp)
  :config
  (when (modulep! +lsp)
    (add-hook 'elixir-ts-mode-local-vars-hook #'lsp! 'append)
    (add-hook 'heex-ts-mode-local-vars-hook #'lsp! 'append)
    (after! lsp-mode
      (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]_build\\'")))

  (when (modulep! +tree-sitter)
    (add-hook 'heex-ts-mode-local-vars-hook #'tree-sitter! 'append)
    (after! tree-sitter
      (add-to-list 'tree-sitter-major-mode-language-alist '(heex-ts-mode . heex))))
  )

(use-package apheleia
  :config
  ;; Use mix-format for HEEx files
  (add-to-list 'apheleia-mode-alist '(heex-ts-mode . mix-format))
  )

(use-package! flycheck-credo
  :when (and (modulep! :checkers syntax)
             (not (modulep! :checkers syntax +flymake)))
  :after elixir-ts-mode
  :config (flycheck-credo-setup))


(use-package exunit
  :hook (elixir-ts-mode . exunit-mode)
  :init
  (map! :after elixir-ts-mode
        :localleader
        :map elixir-ts-mode-map
        :prefix ("t" . "test")
        "a" #'exunit-verify-all
        "r" #'exunit-rerun
        "v" #'exunit-verify
        "T" #'exunit-toggle-file-and-test
        "t" #'exunit-toggle-file-and-test-other-window
        "s" #'exunit-verify-single))
