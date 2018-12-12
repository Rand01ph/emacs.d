(use-package go-mode
  :ensure
  :config
  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'go-mode-hook 'wacky-tab-hook))

(provide 'custom-go)
