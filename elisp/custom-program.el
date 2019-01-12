;;; custom-program.el --- -*- lexical-binding: t; -*-

;;; Commentary:
;;; GNU Emacs configuration file

;;; Code:

;; projects
(use-package projectile
  :ensure t
  :after helm
  :diminish projectile-mode
  :config
  (setq projectile-completion-system 'helm)
  (projectile-mode 1))

(use-package helm-projectile
  :ensure t
  :after (helm projectile)
  :bind ("s-t" . helm-projectile-find-file)
  :config
  (helm-projectile-on))

;; flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; YASnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :config (use-package yasnippet-snippets))

;; yaml
(use-package yaml-mode
  :mode
  (("\\.yml$" . yaml-mode)
   ("\\.yaml$" . yaml-mode)))

(provide 'custom-program)
;;; custom-program.el ends here
