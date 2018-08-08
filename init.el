;;; init.el --- My Emacs configurations.	-*- lexical-binding: t -*-

(require 'package)
;;; packages:
(setq package-archives
      '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
	("melpa" . "http://elpa.emacs-china.org/melpa/")
	("org"   . "http://elpa.emacs-china.org/org/")))

(package-initialize)

;; ensure that use-package is installed
;; use-package
(setq package-enable-at-startup nil)
(unless (package-installed-p 'use-package)
  (progn
    (package-refresh-contents)
    (package-install 'use-package)))

;; Should set before loading `use-package'
(defvar use-package-always-ensure t)

(require 'use-package)

(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))

(require 'custom-const)
(require 'custom-setup)
(require 'custom-ui)

(require 'custom-editor)

(require 'custom-evil)

(require 'custom-program)
(require 'custom-company)

(require 'custom-python)
(require 'custom-rust)
(require 'custom-json)
(require 'custom-javascript)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (pyenv-mode-auto moe-theme lsp-rust pyenv-mode lsp-python company-lsp lsp-ui lsp-mode company-quickhelp company evil-leader evil-escape which-key use-package smex smartparens rainbow-delimiters projectile monokai-theme flycheck exec-path-from-shell evil-surround evil-nerd-commenter diminish counsel anzu))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
