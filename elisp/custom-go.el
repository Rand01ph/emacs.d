;;; custom-go.el --- golang configuration  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package go-mode
  :commands go-mode
  :mode (("\\.go?\\'" . go-mode))
  :defer t
  :init
  (add-hook 'go-mode-hook #'lsp)
  :config
  ;; 缩进设置
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4)
  (setq tab-width 4)
  ;; 保存前 lsp-format-buffer
  (add-hook 'before-save-hook 'lsp-format-buffer))

(provide 'custom-go)
;;; custom-go.el ends here
