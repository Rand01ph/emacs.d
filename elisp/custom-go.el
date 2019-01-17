;;; custom-go.el --- golang configuration  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package go-mode
  :ensure t
  :config
  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'go-mode-hook 'wacky-tab-hook))

(provide 'custom-go)
;;; custom-go.el ends here
