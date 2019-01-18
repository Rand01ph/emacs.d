;;; custom-lsp.el --- lsp configuration

;;; Commentary:

;;; Code:

;; Emacs client for the Language Server Protocol
;; https://github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
  :ensure t
  :commands lsp
  :init
  (setq lsp-auto-guess-root t)    ; 自動选project root
  :config
  (require 'lsp-clients)
  (add-hook 'python-mode-hook #'lsp)
  (add-hook 'go-mode-hook #'lsp)
  )

(use-package company-lsp
  :commands company-lsp)

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;;; company
(use-package company
  :config
  (global-company-mode)
  (push 'company-lsp company-backends))

(provide 'custom-lsp)
;;; custom-lsp.el ends here
