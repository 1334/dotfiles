  ;; better defaults
  (setq inhibit-startup-message t)

  (scroll-bar-mode -1)        ; Disable visible scrollbar
  (tool-bar-mode -1)          ; Disable the toolbar
  (tooltip-mode -1)           ; Disable tooltips
  (set-fringe-mode 10)        ; Give some breathing room

  (menu-bar-mode -1)          ; Disable the menu bar

  ;; Set up the visible bell
  (setq visible-bell nil)

  ;; Make ESC quit prompts
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

  (column-number-mode) ; enables column mode

  (global-display-line-numbers-mode t)

  ;; Disable line numbers for some modes
  (dolist (mode '(org-mode-hook
                  term-mode-hook
                  shell-mode-hook
                  eshell-mode-hook
                  command-log-mode-hook
                  ))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))

  ;; config cursors to dispaly properly on the terminal
  (unless (display-graphic-p)
    (add-hook 'evil-insert-state-entry-hook (lambda () (send-string-to-terminal "\033[5 q")))
    (add-hook 'evil-normal-state-entry-hook (lambda () (send-string-to-terminal "\033[0 q"))))

  (set-face-attribute 'default nil :height 130)

  ;; NOTE: The first time you load your configuration on a new machine, you'll
  ;; need to run the following command interactively so that mode line icons
  ;; display correctly:
  ;;
  ;; M-x all-the-icons-install-fonts
  (use-package all-the-icons)

  (use-package doom-modeline
    :init (doom-modeline-mode 1)
    :custom ((doom-modeline-height 15)))

  ;; (use-package doom-themes
  ;;   :config
  ;;   ;; Global settings (defaults)
  ;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
  ;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;;   (load-theme 'doom-monokai-classic t))

  (use-package monokai-theme :config (load-theme 'monokai t))

  ;; Setup Emacs package manager
  ;; Initialize package sources
  (require 'package)

  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("org" . "https://orgmode.org/elpa/")
                           ("elpa" . "https://elpa.gnu.org/packages/")))

  (package-initialize)

  (unless package-archive-contents
   (package-refresh-contents))

  (use-package rainbow-delimiters
    :hook (prog-mode . rainbow-delimiters-mode))

  (unless (package-installed-p 'use-package)
     (package-install 'use-package))

  (require 'use-package)
  (setq use-package-always-ensure t)

  (defun errstate/evil-hooks ()
    (dolist (mode '(git-rebase-mode
                    eshell-mode
                    term-mode))
      (add-to-list 'evil-emacs-state-modes mode)))

  ;; setup evil
  (use-package evil
    :init
    (setq evil-want-integration t)
    (setq evil-want-keybinding nil)
    (setq evil-want-C-u-scroll t)
    (setq evil-want-C-i-jump nil)
    :hook (evil-mode . errstate/evil-hooks)
    :config
    (evil-mode 1)
    (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

    ;; Use visual line motions even outside of visual-line-mode buffers
    (evil-global-set-key 'motion "j" 'evil-next-visual-line)
    (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

    (evil-set-initial-state 'messages-buffer-mode 'normal)
    (evil-set-initial-state 'dashboard-mode 'normal))

  (use-package evil-collection
    :after evil
    :config
    (evil-collection-init))

  ;; install command log package
  (use-package command-log-mode)

  ;; Install Ivy
  (use-package ivy
    :diminish
    :bind (("C-s" . swiper)
           :map ivy-minibuffer-map
           ("TAB" . ivy-alt-done)
           ("C-l" . ivy-alt-done)
           ("C-j" . ivy-next-line)
           ("C-k" . ivy-previous-line)
           :map ivy-switch-buffer-map
           ("C-k" . ivy-previous-line)
           ("C-l" . ivy-done)
           ("C-d" . ivy-switch-buffer-kill)
           :map ivy-reverse-i-search-map
           ("C-k" . ivy-previous-line)
           ("C-d" . ivy-reverse-i-search-kill))
    :config
    (ivy-mode 1))

  (use-package ivy-rich
    :init
    (ivy-rich-mode 1))

  (use-package counsel
    :bind (("M-x" . counsel-M-x)
           ("C-x b" . counsel-ibuffer)
           ("C-x C-f" . counsel-find-file)
           :map minibuffer-local-map
           ("C-r" . 'counsel-minibuffer-history)))

  (use-package which-key
    :init (which-key-mode)
    :diminish which-key-mode
    :config
    (setq which-key-idle-delay 0.2))

  ;; helpful package
  (use-package helpful
    :custom
    (counsel-describe-function-function #'helpful-callable)
    (counsel-describe-variable-function #'helpful-variable)
    :bind
    ([remap describe-function] . counsel-describe-function)
    ([remap describe-command] . helpful-command)
    ([remap describe-variable] . counsel-describe-variable)
    ([remap describe-key] . helpful-key))

  ;; setup general.el package
  (use-package general
    :config
    (general-create-definer errstate/leader-keys
      :keymaps '(normal insert visual emacs)
      :prefix "SPC"
      :global-prefix "C-SPC")

    (errstate/leader-keys
      "t"  '(:ignore t :which-key "toggles")
      "tt" '(counsel-load-theme :which-key "choose theme")))

  (use-package hydra)

  (defhydra hydra-text-scale (:timeout 4)
    "scale text"
    ("j" text-scale-increase "in")
    ("k" text-scale-decrease "out")
    ("f" nil "finished" :exit t))

  (errstate/leader-keys
    "ts" '(hydra-text-scale/body :which-key "scale text"))

  (use-package projectile
    :diminish projectile-mode
    :config (projectile-mode)
    :custom ((projectile-completion-system 'ivy))
    :bind-keymap
    ("C-c p" . projectile-command-map)
    :init
    ;; NOTE: Set this to the folder where you keep your Git repos!
    (when (file-directory-p "~/code")
      (setq projectile-project-search-path '("~/code")))
    ;; first action to do when switching to a project
    (setq projectile-switch-project-action #'projectile-dired))

  (use-package counsel-projectile
    :config (counsel-projectile-mode))

  (errstate/leader-keys
    "p"  '(:ignore t :which-key "project")
    "pf" '(projectile-find-file :which-key "find file in project")
    "pp" '(projectile-switch-project :which-key "switch project"))

  (use-package magit
    :custom
    (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

  (use-package evil-magit
    :after magit)

  (use-package forge)

  (defun errstate/org-mode-visual-fill ()
    (setq visual-fill-column-width 120
          visual-fill-column-center-text t)
    (visual-fill-column-mode 1))

  (use-package visual-fill-column
    :defer t
    :hook (org-mode . errstate/org-mode-visual-fill))

  (defun isp/lsp-mode-setup ()
    (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
    (lsp-headerline-breadcrumb-mode))

  (use-package lsp-mode
    :commands (lsp lsp-deferred)
    :hook (lsp-mode . isp/lsp-mode-setup)
    :init
    (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
    :config
    (lsp-enable-which-key-integration t))

  (use-package lsp-ui
    :hook (lsp-mode . lsp-ui-mode)
    :custom
    (lsp-ui-doc-position 'bottom))

  (use-package lsp-treemacs
    :after lsp)

  (use-package lsp-ivy)

  (use-package elixir-mode
    :commands lsp
    :diminish lsp-mode
    :hook (elixir-mode . lsp)
    :init
    (setq lsp-enable-snippet nil)
    (add-to-list 'exec-path "~/.elixir-ls/release")
    (add-hook 'elixir-mode-hook
              (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))
    (add-hook 'elixir-format-hook (lambda ()
                                    (if (projectile-project-p)
                                        (setq elixir-format-arguments
                                              (list "--dot-formatter"
                                                    (concat (locate-dominating-file buffer-file-name ".formatter.exs") ".formatter.exs")))
                                      (setq elixir-format-arguments nil))))
    )

  (use-package js2-mode
    :mode "\\.js\\'"
    :init
    (setq js-basic-indent 2)
    (setq-default js2-basic-indent 2
                  js2-basic-offset 2
                  js2-auto-indent-p t))

  (defun isp/org-mode-setup ()
    (org-indent-mode)
    (visual-line-mode 1))

  (use-package org
    :hook (org-mode . isp/org-mode-setup)
    :config
    (setq org-ellipsis " ▾")
    ;; (setq org-agenda-files '())
    ;; (setq org-agenda-start-with-log-mode t)
    ;; (setq org-log-done 'time)
    ;; (setq org-log-intro-drawer t)
    )

  (use-package org-bullets
    :after org
    :hook (org-mode . org-bullets-mode)
    :custom
    (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))


  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (js . t)
     ;; (elixir . t)
     (emacs-lisp . t)
     (ruby . t)
     ;; (typesript . t)
     ))

  ;; Automatically tangle our emacs org config file when we save it
   (defun errstate/org-babel-tangle-config ()
     (when (string-equal (buffer-file-name)
                         (expand-file-name "~/.emacses/self/emacs-config.org"))
       ;; Dynamic scoping to the rescue
       (let ((org-confirm-babel-evaluate nil))
         (org-babel-tangle))))

    (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'errstate/org-babel-tangle-config)))