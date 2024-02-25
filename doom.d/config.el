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
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(set-face-attribute 'default nil :height 130)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       ORG MODE         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(use-package! org
  :custom
  (org-directory "~/org/"))

(use-package! org-roam
  :custom
  (org-roam-directory "~/roam")
  (org-roam-capture-templates
   '(("d" "default" plain "%?" :target
      (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("l" "programming language" plain
      (file "~/roam/templates/programming_language.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+titl
e: ${title}\n")
      :unnarrowed t)
     )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       JS / TS          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-lookup-handlers! 'tide-mode :async t
  :definition #'tide-jump-to-definition)

(setenv "NODE_OPTIONS" "--max-old-space-size=8192")

;; for improving LSP performance
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
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

;; (use-package! tide
;;   :config
;;   (map! :localleader
;;         :map tide-mode-map
;;         :desc "tide fix" :nve "." #'tide-fix))

(add-hook 'typescript-mode-hook
          (lambda () (add-hook 'before-save-hook #'lsp-eslint-apply-all-fixes -99 'local)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       ELIXIR           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after! lsp-mode
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("nextls" "--stdio"))
                    :multi-root t
                    :activation-fn (lsp-activate-on "elixir")
                    :server-id 'next-ls)))

;; (define-derived-mode heex-mode web-mode "HEEx"
;;                      "Major mode for editing HEEx files")

;; (add-to-list 'auto-mode-alist '("\\.heex\\'" . heex-mode))
;; (add-to-list 'auto-mode-alist '("\\.webmanifest\\'" . json-mode))

;; (add-hook 'heex-mode-hook #'lsp)

;; ;; add heex files to html lsp mode
;; (after! lsp-mode (add-to-list 'lsp-language-id-configuration '(heex-mode . "html")))

;; ;; save heex files using the elixir formatter
;; (add-hook 'heex-mode-hook
;;           (lambda () (add-hook 'before-save-hook #'elixir-format nil 'local)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         WEB            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! lsp-tailwindcss
  :init
  (setq lsp-tailwindcss-add-on-mode t)
  :config
  (add-to-list 'lsp-tailwindcss-major-modes 'typescript-tsx-mode :append)
  (add-to-list 'lsp-tailwindcss-major-modes 'elixir-mode :append)
  (add-to-list 'lsp-tailwindcss-major-modes 'heex-mode :append)
  (add-to-list 'lsp-tailwindcss-major-modes 'web-mode :append)
  :after (lsp-mode))

(set-docsets! '(web-mode css-mode rjsx-mode typescript-tsx-mode heex-mode)
  :add "Tailwind_CSS")


(setq! lsp-html-format-enable -1)

(setq-hook! 'html-mode-hook +format-with-lsp nil)
(setq-hook! 'yaml-mode-hook +format-with-lsp nil)
(setq-hook! 'javascript-mode-hook +format-with-lsp nil)
(setq-hook! 'typescript-mode-hook +format-with-lsp nil)

(setq-hook! 'typescript-tsx-mode-hook +format-with-lsp nil)
(setq-hook! 'rjsx-mode-hook +format-with-lsp nil)
(setq-hook! 'json-mode-hook +format-with-lsp nil)

;; (setq-hook! 'javascript-mode-hook +format-with 'prettier)
;; (setq-hook! 'typescript-mode-hook +format-with 'prettier)
;; (setq-hook! 'json-mode-hook +format-with 'prettier)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        COPILOT         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; accept completion from copilot and fallback to company
;; complete by copilot first, then company-mode
(defun my-tab ()
  (interactive)
  (or (copilot-accept-completion)
      (company-indent-or-complete-common nil)))

(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'my-tab)
              ("TAB" . 'my-tab)
              ("C-TAB" . 'my-tab)
              ("C-<tab>" . 'my-tab)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       POLYMODE         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package! polymode
;;   :config
;;   (define-hostmode poly-elixir-hostmode :mode 'elixir-mode)
;;   (define-innermode poly-elixir-innermode
;;     :mode 'heex-mode
;;     :head-matcher (rx line-start (* space) "~H" (= 3 (char "\"")) line-end)
;;     :tail-matcher (rx line-start (* space) (= 3 (char "\"")) line-end)
;;     :head-mode 'host
;;     :tail-mode 'host
;;     :allow-nested nil)
;;   (define-polymode poly-elixir-mode
;;     :hostmode 'poly-elixir-hostmode
;;     :innermodes '(poly-elixir-innermode))
;;   (add-to-list 'auto-mode-alist '("\\.ex?\\'" . poly-elixir-mode))
;;   (add-to-list 'polymode-run-these-after-change-functions-in-other-buffers 'lsp-on-change)
;;   (add-to-list 'polymode-run-these-before-change-functions-in-other-buffers 'lsp-before-change)
;;   )

;; workaround for polymode described
;; https://github.com/polymode/polymode/issues/316
;; (setq-hook! elixir-mode polymode-lsp-integration nil)
;; (setq-hook! heex-mode polymode-lsp-integration nil)

;; (use-package! polymode
;;   :config
;;   (define-hostmode poly-rjsx-hostmode :mode 'rjsx-mode)
;;   (define-innermode poly-rjsx-cssinjs-innermode
;;     :mode 'css-mode
;;     :head-matcher "\\(styled\\|css\\)[.()<>[:alnum:]]?+`"
;;     :tail-matcher "\`"
;;     :head-mode 'host
;;     :tail-mode 'host
;;     :keep-in-mode 'host
;;     :fallback-mode 'host)
;;   (define-polymode poly-rjsx-mode
;;     :hostmode 'poly-rjsx-hostmode
;;     :innermodes '(poly-rjsx-cssinjs-innermode))
;;   :hook
;;   ((rjsx-mode) . poly-rjsx-mode)
;;   )
