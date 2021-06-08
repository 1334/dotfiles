;; Automatically tangle our Emacs.org config file when we save it
 (defun isp/org-babel-tangle-config ()
   (when (string-equal (buffer-file-name)
                       (expand-file-name "~/.emacs-d/emacs-conf.org"))
     ;; Dynamic scoping to the rescue
     (let ((org-confirm-babel-evaluate nil))
       (org-babel-tangle))))

  (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'isp/org-babel-tangle-config)))
