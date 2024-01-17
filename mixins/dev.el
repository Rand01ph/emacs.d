;;; dev.el --- Development tools                    -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Version Control
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Magit: best Git client to ever exist
(use-package magit
  :bind (("s-g" . magit-status)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Tree-sitter, the built-in language highlight for Emacs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Tree-sitter support
;; https://git.savannah.gnu.org/cgit/emacs.git/tree/admin/notes/tree-sitter/starter-guide?h=emacs-29
(use-package treesit-auto
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Eglot, the built-in LSP client for Emacs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package eglot
  :defer t
  :commands (eglot-ensure)
  :hook
  ((python-mode 
    go-mode) . eglot-ensure)
  :bind (:map eglot-mode-map
              ("C-c l a" . eglot-code-actions)
              ("C-c l r" . eglot-rename)
              ("C-c l f" . eglot-format)
              ("C-c l d" . eldoc))
  :config
  ;; 将 eglot-events-buffer-size 设置为 0 后将关闭显示 *EGLOT event* bufer，不便于调试问题。
  ;; 也不能设置的太大，否则可能影响性能。
  (setq eglot-events-buffer-size 1000)
  ;; 将 flymake-no-changes-timeout 设置为 nil 后，eglot 在保存 buffer 内容后，经过 idle time 才会显示 LSP 发送
  ;; 的诊断消息。
  (setq eglot-send-changes-idle-time 0.3)

  ;; Shutdown server when last managed buffer is killed
  (customize-set-variable 'eglot-autoshutdown t)
  (customize-set-variable 'eglot-connect-timeout 60)   ;; default 30s

  (setq read-process-output-max (* 1024 1024))
  (setq eglot-events-buffer-size 0)

  ;; 忽略一些用不到，耗性能的能力。
  (setq eglot-ignored-server-capabilities
	'(
	  ;;:hoverProvider ;; 显示光标出信息。
	  ;;:documentHighlightProvider ;; 高亮当前 symbol。
	  :inlayHintProvider ;; 显示 inlay hint 提示。
	  ))

  ;; 加强高亮的 symbol 效果。
  (set-face-attribute 'eglot-highlight-symbol-face nil
                      :background "#b3d7ff")

  ;; t: true, false: :json-false 而不是 nil。
  (setq-default eglot-workspace-configuration
		'((:gopls .
			  ((staticcheck . t)
			   (usePlaceholders . :json-false)
			   (matcher . "CaseSensitive")))))

  (defclass pylance-lsp (eglot-lsp-server) ()
   :documentation "pylance Language Server.")

  (cl-defmethod eglot-initialization-options ((server pylance-lsp))
   "Passes the initializationOptions required to run
   the server."
   `(:server-id ,"pylance"))

  (add-to-list 'eglot-server-programs
	       '(go-mode . ("gopls")))
  (add-to-list 'eglot-server-programs
	       '(shell-script-mode . ("bash-language-server")))
  (add-to-list 'eglot-server-programs
	       '((python-mode python-ts-mode) pylance-lsp "eglot-pylance")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Common file types
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode
  :hook ((markdown-mode . visual-line-mode)))

(use-package yaml-mode)

(use-package json-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Python
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package python
  :hook ((python-mode . eglot-ensure)
         (python-mode . corfu-mode))
  :mode ("\\.py" . python-mode)
  :config
  (setq python-indent-offset 4))

(use-package pyenv-mode
  :init
  (add-to-list 'exec-path "~/.pyenv/shims")
  (setenv "WORKON_HOME" "~/.pyenv/versions/")
  :config
  (pyenv-mode t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Shell
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package shell-script-mode
  :ensure nil
  :hook (shell-script-mode . eglot-ensure)
  :mode
  "\\(.sh\\|.bash\\|.zsh\\)\\(_history\\|_profile\\|rc\\)")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Go
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package go-mode
  :after eglot
  :mode "\\.go'"
  :config
  (defun my-eglot-organize-imports () (interactive)
	 (eglot-code-actions nil nil "source.organizeImports" t))
  (defun eglot-buffer-on-save ()
    (add-hook 'before-save-hook #'my-eglot-organize-imports nil t)
    (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
    )
  (add-hook 'go-mode-hook #'eglot-buffer-on-save)
  (add-hook 'before-save-hook 'gofmt-before-save))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   JSX/TSX
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package jtsx
  :ensure t
  :mode (("\\.jsx?\\'" . jtsx-jsx-mode)
         ("\\.tsx?\\'" . jtsx-tsx-mode))
  :commands jtsx-install-treesit-language
  :hook ((jtsx-jsx-mode . hs-minor-mode)
         (jtsx-tsx-mode . hs-minor-mode))
  :custom
  ;; Optional customizations
  ;; (js-indent-level 2)
  ;; (typescript-ts-mode-indent-offset 2)
  ;; (jtsx-switch-indent-offset 0)
  ;; (jtsx-indent-statement-block-regarding-standalone-parent nil)
  ;; (jtsx-jsx-element-move-allow-step-out t)
  ;; (jtsx-enable-jsx-electric-closing-element t)
  ;; (jtsx-enable-electric-open-newline-between-jsx-element-tags t)
  (jtsx-enable-all-syntax-highlighting-features t)
  :config
  (defun jtsx-bind-keys-to-mode-map (mode-map)
    "Bind keys to MODE-MAP."
    (define-key mode-map (kbd "C-c C-j") 'jtsx-jump-jsx-element-tag-dwim)
    (define-key mode-map (kbd "C-c j o") 'jtsx-jump-jsx-opening-tag)
    (define-key mode-map (kbd "C-c j c") 'jtsx-jump-jsx-closing-tag)
    (define-key mode-map (kbd "C-c j r") 'jtsx-rename-jsx-element)
    (define-key mode-map (kbd "C-c <down>") 'jtsx-move-jsx-element-tag-forward)
    (define-key mode-map (kbd "C-c <up>") 'jtsx-move-jsx-element-tag-backward)
    (define-key mode-map (kbd "C-c C-<down>") 'jtsx-move-jsx-element-forward)
    (define-key mode-map (kbd "C-c C-<up>") 'jtsx-move-jsx-element-backward)
    (define-key mode-map (kbd "C-c C-S-<down>") 'jtsx-move-jsx-element-step-in-forward)
    (define-key mode-map (kbd "C-c C-S-<up>") 'jtsx-move-jsx-element-step-in-backward)
    (define-key mode-map (kbd "C-c j w") 'jtsx-wrap-in-jsx-element))
    
  (defun jtsx-bind-keys-to-jtsx-jsx-mode-map ()
      (jtsx-bind-keys-to-mode-map jtsx-jsx-mode-map))

  (defun jtsx-bind-keys-to-jtsx-tsx-mode-map ()
      (jtsx-bind-keys-to-mode-map jtsx-tsx-mode-map))

  (add-hook 'jtsx-jsx-mode-hook 'jtsx-bind-keys-to-jtsx-jsx-mode-map)
  (add-hook 'jtsx-tsx-mode-hook 'jtsx-bind-keys-to-jtsx-tsx-mode-map))
