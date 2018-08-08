
;; Python support for lsp-mode using pyls.
;; Install: pip install 'python-language-server[all]'
(use-package lsp-python
  :commands lsp-python-enable
  :hook (python-mode . lsp-python-enable))

;; Python
(use-package pyenv-mode
  :init
  (add-to-list 'exec-path "~/.pyenv/shims")
  (setenv "WORKON_HOME" "~/.pyenv/versions/")
  :config
  (pyenv-mode)
  :hook python-mode)

(use-package pyenv-mode-auto)

(provide 'custom-python)
