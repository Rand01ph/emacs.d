;;; init.el --- Rand01ph's Emacs configurations.   -*- lexical-binding: t -*- no-byte-compile: t; -*-

;; Version: 0.0.1

;;; Commentary:
;; Emacs 配置,缓慢进化中
;; init configurations.
;;

;;; Code:

;;; config package manager
(require 'package)
(setq package-archives
      '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
	("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
	("org"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))
(package-initialize)

;; bootstrap use-package
;; ensure that use-package is installed
(setq package-enable-at-startup nil)
(unless (package-installed-p 'use-package)
  (progn
    (package-refresh-contents)
    (package-install 'use-package)))

;; Should set before loading `use-package'
(defvar use-package-always-ensure t)

(require 'use-package)

(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))

;; Constants
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-linux* (eq system-type 'gnu/linux))
(defconst *is-a-windows* (eq system-type 'windows-nt))
(defconst *is-gui* (display-graphic-p))
(defconst *is-console* (not *is-gui*))

;; 开启内置行号
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;; common
(setq create-lockfiles nil)
(fset 'yes-or-no-p 'y-or-n-p)

(setq ring-bell-function 'ignore)	; No bell please

;; Auto-save and Backups
;; (setq auto-save-default nil)
(setq auto-save-file-name-transforms
    `((".*" ,temporary-file-directory t)))
(setq make-backup-files t
    version-control t
    backup-by-copying t
    kept-old-versions 2
    kept-new-versions 5
    delete-old-versions t)
(setq backup-directory-alist
    `((".*" . ,temporary-file-directory)))

;;; exec-path-from-shell
(use-package exec-path-from-shell
  :if (memq window-system '(ns mac x))
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("PATH" "GOPATH")))

;; 外观配置
;;; Disable toolbar & menubar & scroll-bar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; 背景透明
;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;;(set-frame-parameter (selected-frame) 'alpha <both>)
(set-frame-parameter (selected-frame) 'alpha '(85 . 85))
(add-to-list 'default-frame-alist '(alpha . (85 . 85)))

;; theme
(use-package moe-theme
  :ensure t
  :config
  (load-theme 'moe-dark t))

;; 字体配置
;; M+ 1m    Noto Sans CJK SC
(defun my-default-font()
  "Config my font."
  (interactive)
  (setq fonts
        (cond ((eq system-type 'darwin)     '("Monaco"           "STHeiti"))
              ((eq system-type 'gnu/linux)  '("M+ 1m"            "Noto Sans CJK SC"))
              ((eq system-type 'windows-nt) '("DejaVu Sans Mono" "Microsoft Yahei"))))
  (set-face-attribute 'default nil :font
                      (format "%s:pixelsize=%d" (car fonts) 14))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family (car (cdr fonts)) :size 12)))
  ;; Fix chinese font width and rescale
  (setq face-font-rescale-alist '(("STHeiti" . 1.2) ("STFangsong" . 1.2) ("Microsoft Yahei" . 1.2) ("Noto Sans CJK SC" . 1.2)))
  )

(add-to-list 'after-make-frame-functions
         (lambda (new-frame)
           (select-frame new-frame)
           (if window-system
           (my-default-font)
         )))

(if window-system
    (my-default-font)
  )

;;; evil config
(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1))

(use-package evil-escape
  :ensure t
  :after evil ;; 表示在evil载入后才能载入
  :config
  (progn
    (setq-default evil-escape-key-sequence "kj")
    (setq-default evil-escape-delay 0.2)
    (evil-escape-mode t)
    ))

(use-package evil-leader
  :ensure t
  :after evil
  :commands (evil-leader/set-leader)
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>"))

(use-package evil-surround
  :ensure t
  :config
  (progn
    (global-evil-surround-mode t)
    ))

(use-package evil-nerd-commenter
  :after evil
  :defer t
  :ensure t
  :commands (evilnc-comment-or-uncomment-lines)
  :init
  (define-key evil-normal-state-map (kbd "s-/") 'evilnc-comment-or-uncomment-lines)
  (define-key evil-visual-state-map (kbd "s-/") 'evilnc-comment-or-uncomment-lines)
)

;;; lsp-mode
(use-package lsp-mode
  :commands lsp)
(use-package company-lsp)
(use-package lsp-ui
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;;; company
(use-package company
  :commands (global-company-mode)
  :config
  (global-company-mode)
  (push 'company-lsp company-backends))

;;; flycheck
(use-package flycheck
  :commands (global-flycheck-mode)
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode))

;;; yaml-mode
(use-package yaml-mode
  :mode
  (("\\.yml$" . yaml-mode)
   ("\\.yaml$" . yaml-mode)))

;;; json-mode
(use-package json-mode
  :mode ("\\.json\\'" . json-mode))

;; python
(use-package python-mode
  :defer t
  :commands python-mode
  :config
  (add-hook 'python-mode-hook #'lsp))

;; golang environment
(use-package go-mode
  :commands go-mode
  :mode (("\\.go?\\'" . go-mode))
  :defer t
  :init
  (add-hook 'go-mode-hook #'lsp)
  :config
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (add-hook 'before-save-hook 'lsp-format-buffer))

;; lua environment
(use-package lua-mode
  :ensure t
  :config
  (setq lua-indent-level 4))

;; PHP environment
(use-package php-mode
  :ensure t
  :mode (("\\.php" . php-mode)
	 ("\\.phtml" . php-mode))
  :commands php-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (python-mode yasnippet-snippets yaml-mode which-key web-mode use-package smartparens rainbow-delimiters pyenv-mode prettier-js php-mode moe-theme lua-mode lsp-ui lsp-rust lsp-javascript-typescript lsp-html lsp-css kubernetes-tramp kubernetes json-mode js2-refactor helm-rg helm-projectile go-mode flycheck exec-path-from-shell evil-surround evil-nerd-commenter evil-leader evil-escape emmet-mode diminish company-lsp auto-package-update anzu))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
