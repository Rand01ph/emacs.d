
;; Python support for lsp-mode using pyls.
;; Install: pip install 'python-language-server[all]'
(use-package lsp-python
  :commands lsp-python-enable
  :after (lsp-mode)
  :hook (python-mode . lsp-python-enable)
  :config
  ;; Replace with my own traverser for now, as the current one is buggy.
  (lsp-define-stdio-client lsp-python "python"
			   (lsp-make-traverser
			    #'(lambda (dir)
				(directory-files
				 dir
				 nil
				 "\\(__init__\\|setup\\|manage\\)\\.py\\|requirements.txt")))
			   '("pyls")))

;; Python
(use-package pyenv-mode
  :init
  (add-to-list 'exec-path "~/.pyenv/shims")
  (setenv "WORKON_HOME" "~/.pyenv/versions/")
  :config
  (pyenv-mode)
  :hook python-mode)

(use-package pyenv-mode-auto
  :ensure t)

(provide 'custom-python)
