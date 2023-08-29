(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
    '("835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" default))
 '(package-selected-packages '(js-import import-js))
 '(safe-local-variable-values
    '((lexical-bindinvag . t)
       (eval let
         ((project-directory
            (car
              (dir-locals-find-file default-directory))))
         (setq lsp-clients-typescript-server-args
           `("--tsserver-path" ,(concat project-directory ".yarn/sdks/typescript/bin/tsserver")
              "--stdio")))))
 '(warning-suppress-types '((after-save-hook))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ts-fold-replacement-face ((t (:foreground nil :box nil :inherit font-lock-comment-face :weight light)))))
