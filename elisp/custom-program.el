
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

;; YASnippet
(use-package yasnippet
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :config
  (yas-reload-all))

(provide 'custom-program)
