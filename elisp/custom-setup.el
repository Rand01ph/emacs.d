;;; custom-setup.el --- Summary
;;; Commentary:

;;; Code:
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;; common
(setq create-lockfiles nil)
(fset 'yes-or-no-p 'y-or-n-p)

(setq ring-bell-function 'ignore)	; No bell please

;; Auto-save and Backups
;; (setq auto-save-default nil)
(setq auto-save-file-name-transforms
    `((".*" ,temporary-file-directory t)))
(setq make-backup-files t
    version-control t
    backup-by-copying t
    kept-old-versions 2
    kept-new-versions 5
    delete-old-versions t)
(setq backup-directory-alist
    `((".*" . ,temporary-file-directory)))


(use-package exec-path-from-shell
  :if (memq window-system '(ns mac))
  :ensure t
  :config (exec-path-from-shell-initialize))

(use-package auto-package-update
   :ensure t
   :config
   (setq auto-package-update-delete-old-versions t
         auto-package-update-interval 4)
   (auto-package-update-maybe))

(provide 'custom-setup)
;;; custom-setup.el ends here
