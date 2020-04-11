;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Iñigo Solano"
      user-mail-address "i@errstate.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-dark+)
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; config cursors to dispaly properly on the terminal
(unless (display-graphic-p)
  (add-hook 'evil-insert-state-entry-hook (lambda () (send-string-to-terminal "\033[5 q")))
  (add-hook 'evil-normal-state-entry-hook (lambda () (send-string-to-terminal "\033[0 q"))))

;; config elixir-ls
(setq lsp-clients-elixir-server-executable "~/.elixir_ls_releases/language_server.sh")

;; Setup some keybindings for exunit and lsp-ui
(map! :mode elixir-mode
      :leader
      :desc "Toggle file/test" :nve "ctf" #'exunit-toggle-file-and-test
      :desc "Run all tests"   :nve  "cta"   #'exunit-verify-all
      :desc "Run all in umbrella"   :nve  "ctA"   #'exunit-verify-all-in-umbrella
      :desc "Re-run tests"   :nve  "ctr"   #'exunit-rerun
      :desc "Run single test"   :nve  "ctt"   #'exunit-verify-single)

;; disable projectile cleverness for detecting projects
;; tip by @Henrik
;; https://discordapp.com/channels/406534637242810369/603399769015975996/697352633186386110
(after! projectile
  (remove-hook 'projectile-project-root-files-functions #'projectile-root-top-down))

;; custom key binds
;; (map! :leader :desc "Treemacs" "p t" #'treemacs)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.

(set-popup-rules!
  '(("^\\*exunit" :ignore t)))

;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.
