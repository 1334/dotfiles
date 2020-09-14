(setq delete-old-versions -1 )		; delete excess backup versions silently
(setq version-control t )		; use version control
(setq vc-make-backup-files t )		; make backups file even when in version controlled dir
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) ) ; which directory to put backups file
(setq vc-follow-symlinks t )				       ; don't ask for confirmation when opening symlinked file
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ) ;transform backups file name
(setq inhibit-startup-screen t )	; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore )	; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8 )	; use utf-8 by default
(setq coding-system-for-write 'utf-8 )
(setq sentence-end-double-space nil)	; sentence SHOULD end with only a point.
(setq default-fill-column 80)		; toggle wrapping text at the 80th character
(setq initial-scratch-message "Welcome in Emacs") ; print a default message in the empty scratch buffer opened at startup
					
(require 'package)
(setq package-enable-at-startup nil) ; tells emacs not to load any packages before starting up
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; updage packages archive
  (package-install 'use-package)) ; and install the most recent version of use-package

(require 'use-package)
(use-package general :ensure t)
(use-package evil :ensure t :config (evil-mode 1))
(use-package magit :ensure t)
(use-package monokai-theme :config (load-theme 'monokai t) :ensure t)
;; (use-package beacon :ensure t :config (beacon-mode 1)) ;; This applies a beacon effect to the highlighted line
;; (use-package helm
;;   :init
;;     (require 'helm-config)
;;     (setq helm-split-window-in-side-p t
;;           helm-move-to-line-cycle-in-source t)
;;   :config 
;;     (helm-mode 1) ;; Most of Emacs prompts become helm-enabled
;;     (helm-autoresize-mode 1) ;; Helm resizes according to the number of candidates
;;     (global-set-key (kbd "C-x b") 'helm-buffers-list) ;; List buffers ( Emacs way )
;;     (define-key evil-ex-map "b" 'helm-buffers-list) ;; List buffers ( Vim way )
;;     (global-set-key (kbd "C-x r b") 'helm-bookmarks) ;; Bookmarks menu
;;     (global-set-key (kbd "C-x C-f") 'helm-find-files) ;; Finding files with Helm
;;     (global-set-key (kbd "M-c") 'helm-calcul-expression) ;; Use Helm for calculations
;;     (global-set-key (kbd "C-s") 'helm-occur)  ;; Replaces the default isearch keybinding
;;     (global-set-key (kbd "C-h a") 'helm-apropos)  ;; Helmized apropos interface
;;     (global-set-key (kbd "M-x") 'helm-M-x)  ;; Improved M-x menu
;;     (global-set-key (kbd "M-y") 'helm-show-kill-ring)  ;; Show kill ring, pick something to paste
;;   :ensure t)

(global-hl-line-mode t) ;; This highlights the current line in the buffer

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("f3ab34b145c3b2a0f3a570ddff8fabb92dafc7679ac19444c31058ac305275e1" default))
 '(package-selected-packages '(helm magit beacon monokai-theme evil general use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

