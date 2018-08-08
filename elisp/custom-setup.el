
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;; common
(setq create-lockfiles nil)
(fset 'yes-or-no-p 'y-or-n-p)

(setq ring-bell-function 'ignore)	; No bell please

(use-package exec-path-from-shell
  :if (memq window-system '(ns mac))
  :ensure t
  :config (exec-path-from-shell-initialize))

(provide 'custom-setup)
