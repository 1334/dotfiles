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
(setq doom-font (font-spec :family "JetBrains Mono" :size 14 :weight 'light)
      doom-variable-pitch-font (font-spec :family "JetBrains Mono" :size 14 :weight 'light))
;; Increase line spacing
(setq-default line-spacing 0.11)
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
;; (set-face-attribute 'default nil :height 130)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord)

(when (string-equal doom-theme 'doom-nord)
  (custom-set-faces!
    '(font-lock-comment-face :foreground "#b0b0b0")
    '(font-lock-doc-face :foreground "#b0b0b0")))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       GENERAL          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; highlight current line
(hl-line-mode 1)
;; don't want cursor to blink like a maniac
(blink-cursor-mode -1)

;; Revert buffers whne the underlying file has changed
(global-auto-revert-mode 1)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; make avy jump all windows
(setq avy-all-windows t)

(setq! debug-on-error nil)

;; use native MacOS fullscreen
(setq ns-use-native-fullscreen t)
(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)

(map! :leader
      ;; map spc jj to what it used to be in spacemacs
      :desc "goto" :nve "jj" (cmd! (let ((current-prefix-arg t)) (evil-avy-goto-char-timer)))
      ;; show flycheck errors for the current file
      :desc "list errors" :nve "cf" #'list-flycheck-errors
      )

(map! "C-}" #'centaur-tabs-forward)
(map! "C-{" #'centaur-tabs-backward)
(map! "C-M-{" #'centaur-tabs-move-current-tab-to-left)
(map! "C-M-}" #'centaur-tabs-move-current-tab-to-right)
(map! :i "M-RET" #'company-complete-selection)

(map! :n "gV" #'xref-find-definitions-other-window) ;; also mapped to C-x 4 .

(setq major-mode-remap-alist
      '((elixir-mode . elixir-ts-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       ORG MODE         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! org
  :custom
  (org-directory "~/org/"))

(use-package! org-roam
  :custom
  (org-roam-directory "~/roam"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       JS / TS          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setenv "NODE_OPTIONS" "--max-old-space-size=8192")

;; for improving LSP performance
(setq gc-cons-threshold 200000000)
(setq read-process-output-max (* 10 1024 1024)) ;; 10mb
(setq! lsp-clients-typescript-max-ts-server-memory 8092)

(after! lsp-mode
  (setq! lsp-eslint-auto-fix-on-save t))

(map! :after js2-mode
      :localleader
      :map js2-mode-map
      (:prefix ("t" . "test")
       :desc "jest file dwim" :nve "t" #'jest-file-dwim
       :desc "repeat last test" :nve "r" #'jest-repeat
       :desc "test at line" :nve "l" #'jest-function
       :desc "all" :nve "a" #'jest))

(map! :after typescript-mode
      :localleader
      :map typescript-mode-map
      (:prefix ("t" . "test")
       :desc "jest file dwim" :nve "t" #'jest-file-dwim
       :desc "repeat last test" :nve "r" #'jest-repeat
       :desc "test at line" :nve "l" #'jest-function
       :desc "all" :nve "a" #'jest))

(mapc
 (lambda (language-mode-hook)
   (add-hook language-mode-hook
             (lambda () (add-hook 'before-save-hook #'lsp-eslint-apply-all-fixes -99 'local))))
 '(typescript-mode-hook typescript-tsx-mode-hook))

;; use typescript mode for mts files
(add-to-list 'auto-mode-alist '("\\.mts?\\'" . typescript-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       ELIXIR           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Currently elixir congfig is in a custom doom module in modules/lang/elixir
(add-to-list 'auto-mode-alist '("\\.heex?\\'" . heex-ts-mode))

;; disable eslint lsp for heex buffers
(after! lsp-mode
  ;; Override the client activation for eslint in heex-ts-mode
  (add-to-list 'lsp-disabled-clients '(heex-ts-mode . eslint)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         WEB            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! lsp-tailwindcss
  :init (setq lsp-tailwindcss-add-on-mode t)
  :config
  (dolist (tw-major-mode
           '(css-mode
             css-ts-mode
             typescript-mode
             typescript-ts-mode
             typescript-tsx-mode
             tsx-ts-mode
             js2-mode
             js-ts
             elixir-ts-mode
             heex-ts-mode
             clojure-mode))
    (add-to-list 'lsp-tailwindcss-major-modes tw-major-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        COPILOT         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       POLYMODE         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package! polymode
;;   :config
;;   ;; Define the Elixir polymode
;;   (define-hostmode poly-elixir-hostmode
;;     :mode 'elixir-ts-mode)

;;   ;; Define an innermode for the ~H""" ... """ blocks using web-mode
;;   (define-innermode poly-elixir-html-innermode
;;     :mode 'heex-ts-mode
;;     :head-matcher "~H\"\"\""
;;     :tail-matcher "\"\"\""
;;     :head-mode 'host
;;     :tail-mode 'host
;;     :allow-nested nil
;;     :indent-offset 2)

;;   ;; Define the polymode that ties everything together
;;   (define-polymode poly-elixir-mode
;;     :hostmode 'poly-elixir-hostmode
;;     :innermodes '(poly-elixir-html-innermode))

;;   ;; Enable the polymode in elixir-ts-mode buffers
;;   (add-hook 'elixir-ts-mode-hook #'poly-elixir-mode))
