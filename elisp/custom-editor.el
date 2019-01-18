(use-package which-key
	:ensure t
	:config
	(which-key-mode))

(use-package helm
  :ensure t
  :init
  (setq helm-candidate-number-limit 50
        helm-display-header-line nil
        helm-ff-auto-update-initial-value nil
        helm-find-files-doc-header nil
        helm-split-window-in-side-p t
        helm-buffers-fuzzy-matching t
        helm-move-to-line-cycle-in-source t)
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
	 ("C-x b" . helm-buffers-list)
	 ("C-x f" . helm-recentf)
	 ("C-x C-f" . helm-find-files)
	 ("<tab>" . helm-execute-persistent-action)
	 ("C-i" . helm-excute-persistent-action)
	 ("C-z" . helm-select-action))
  :config
  (helm-mode 1))

(use-package helm-rg
  :ensure t)

(use-package smartparens-config
  :ensure smartparens
  :config
  (progn
    (show-smartparens-global-mode t)))

(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
(add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)


(use-package rainbow-delimiters
  :ensure t
  :config
  ;; Only load rainbow-delimiters-mode in prog-mode so it doesn't
  ;; break diffs in magit.
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t))) 

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(provide 'custom-editor)
;;; custom-editor.el ends
