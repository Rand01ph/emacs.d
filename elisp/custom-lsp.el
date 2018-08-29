
;; Emacs client for the Language Server Protocol
;; https://github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
  :ensure t
  :defer t
  :diminish lsp-mode
  :config
  (setq lsp-inhibit-message t)
  (setq lsp-message-project-root-warning t)
  (require 'lsp-imenu)
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)

  ;; https://emacs-china.org/t/topic/6392/2
  (defun restart-lsp-server ()
    "Restart LSP server."
    (interactive)
    (setq lsp--workspaces (make-hash-table :test #'equal))
    (revert-buffer t t)
    (message "LSP server restarted."))

  (lsp-define-stdio-client lsp-python "python"
			   (lsp-make-traverser #'(lambda (dir)
						   (directory-files
						    dir
						    nil
						    "\\(__init__\\|setup\\)\\.py")))
			   '("pyls"))
  )

(use-package lsp-ui
  :bind (:map lsp-ui-mode-map
	      ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
	      ([remap xref-find-references] . lsp-ui-peek-find-references))
  :hook (lsp-mode . lsp-ui-mode))

(provide 'custom-lsp)
