(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :bind (:map js2-mode-map
         ("<return>" . newline-and-indent))
  :config
  (setq js2-basic-offset 2))

;; The JavaScript LSP is required in order to use
;; lsp-javascript-typescript mode. In order to install, type:
;; npm i -g javascript-typescript-langserver
(use-package lsp-javascript-typescript
  :ensure t
  :after (lsp-mode)
  :config
  (add-hook 'js-mode-hook 'lsp-javascript-typescript-enable))

(provide 'custom-javascript)
