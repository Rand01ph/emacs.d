
;; Improved JavaScript editing mode
(use-package js2-mode
  :ensure t
  :defines flycheck-javascript-eslint-executable
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :interpreter (("node" . js2-mode)
                ("node" . js2-jsx-mode))
  :hook ((js2-mode . js2-imenu-extras-mode)
         (js2-mode . js2-highlight-unused-variables-mode)))

(use-package js2-refactor
  :ensure t
  :diminish js2-refactor-mode
  :hook (js2-mode . js2-refactor-mode))

(use-package web-mode
  :ensure t
  :mode ("\\.html?\\'" . web-mode)
  :config
  (setq web-mode-enable-auto-pairing t
		web-mode-enable-css-colorization t
		web-mode-enable-current-element-highlight t))

(use-package emmet-mode
  :ensure t
  :hook (emmet-mode . (css-mode-hook
                       html-mode-hook
                       sass-mode-hook
                       scss-mode-hook
                       web-mode-hook)))

(use-package lsp-html
  :ensure t
  :commands lsp-html-enable
  :hook ((html-mode . lsp-html-enable)
		 (web-mode . lsp-html-enable)))

(use-package lsp-javascript-typescript
  :ensure t
  :commands lsp-javascript-typescript-enable
  :hook ((typescript-mode js2-mode) . lsp-javascript-typescript-enable))

(use-package lsp-css
  :ensure t
  :commands (lsp-css-enable
             lsp-less-enable
             lsp-sass-enable
             lsp-scss-enable)
  :hook ((css-mode . lsp-css-enable)
         (less-mode . lsp-less-enable)
         (sass-mode . lsp-sass-enable)
         (scss-mode . lsp-scss-enable)))

;; format
(use-package prettier-js
  :ensure t
  :commands prettier-js
  :hook ((js2-mode . prettier-js-mode)
		 (json-mode . prettier-js-mode)
		 (yaml-mode . prettier-js-mode)
		 (typescript-mode . prettier-js-mode)))

;; typescript
(use-package typescript-mode
  :ensure t)

(provide 'custom-web)
