
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
  :after
  helm
  projectile
  :bind ("s-t" . helm-projectile-find-file)
  :config
  (helm-projectile-on))

;; flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(provide 'custom-program)
