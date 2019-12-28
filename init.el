;;; init.el --- Rand01ph's Emacs configurations.  -*- lexical-binding: t; no-byte-compile: nil; -*-

;; Version: 0.0.5

;;; Commentary:
;; Emacs 配置,缓慢进化中
;; init configurations.
;;

;;; Code:

;;; Basic Setup
(setq user-full-name "Rand01ph"
	  user-mail-address "tanyawei1991@gmail.com")

;;; 定时GC
(setq gc-cons-threshold (* 20 1024 1024))
(setq large-file-warning-threshold 100000000)

(defmacro k-time (&rest body)
  "Measure and return the time it takes evaluating BODY."
  `(let ((time (current-time)))
	 ,@body
	 (float-time (time-since time))))

(defvar k-gc-timer
  (run-with-idle-timer 15 t
			   (lambda ()
			 (message "Garbage Collector has run for %.06fsec"
				  (k-time (garbage-collect))))))

;;; 编码配置
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;;; 配置 package.el 源
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
	  '(("gnu"   . "https://mirrors.cloud.tencent.com/elpa/gnu/")
		("melpa" . "https://mirrors.cloud.tencent.com/elpa/melpa/")
		("org"   . "https://mirrors.cloud.tencent.com/elpa/org/")))
(package-initialize)

;; bootstrap use-package
;; ensure that use-package is installed
(unless (package-installed-p 'use-package)
  (progn
	(package-refresh-contents)
	(package-install 'use-package)))

;; Should set before loading `use-package'
(defvar use-package-always-ensure t)

(eval-when-compile
  (require 'use-package))

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))

;; Constants
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-linux* (eq system-type 'gnu/linux))
(defconst *is-a-windows* (eq system-type 'windows-nt))
(defconst *is-gui* (display-graphic-p))
(defconst *is-console* (not *is-gui*))

;; 开启内置行号
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))

;; common
(setq create-lockfiles nil)
(fset 'yes-or-no-p 'y-or-n-p)

;; 设置tab宽度
(setq-default tab-width 4)

(setq ring-bell-function 'ignore)	; No bell please

;; 设置行宽度，文本120，程序120
(dolist (hook '(text-mode-hook latex-mode-hook tex-mode-hook))
  (add-hook hook (lambda () (set-fill-column 120))))
(dolist (hook '(python-mode-hook prog-mode-hook list-mode-hook))
  (add-hook hook (lambda () (set-fill-column 120))))

;; 核心模块
;; files and directories
(use-package f
  :ensure t)

;; 搜索高亮
(use-package anzu
	:config
	(global-anzu-mode +1)
	(global-set-key [remap query-replace] 'anzu-query-replace)
	(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp))

;; 开启列号
(setq column-number-mode 1)

;; 停止光标闪烁
(blink-cursor-mode -1)

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

(add-hook 'before-save-hook 'whitespace-cleanup)
(remove-hook 'jsonnet-mode 'whitespace-cleanup t)

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
(set-window-scroll-bars (minibuffer-window) nil nil)

;; 在window中使用完整单文件路径名称
(setq frame-title-format
	  '((:eval (if (buffer-file-name)
				   (abbreviate-file-name (buffer-file-name))
				 "%b"))))


;; 背景透明
;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;;(set-frame-parameter (selected-frame) 'alpha <both>)
(set-frame-parameter (selected-frame) 'alpha '(85 . 85))
(add-to-list 'default-frame-alist '(alpha . (85 . 85)))

;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   (load-theme 'doom-one t)
;;   (doom-themes-visual-bell-config))

(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t)
  (let ((line (face-attribute 'mode-line :underline)))
	(set-face-attribute 'mode-line          nil :overline   line)
	(set-face-attribute 'mode-line-inactive nil :overline   line)
	(set-face-attribute 'mode-line-inactive nil :underline  line)
	(set-face-attribute 'mode-line          nil :box        nil)
	(set-face-attribute 'mode-line-inactive nil :box        nil)
	(set-face-attribute 'mode-line-inactive nil :background "#282C34")))

;; (use-package zenburn-theme
;;	:config
;;	(load-theme 'zenburn t)
;;	(let ((line (face-attribute 'mode-line :underline)))
;;		(set-face-attribute 'mode-line          nil :overline   line)
;;		(set-face-attribute 'mode-line-inactive nil :overline   line)
;;		(set-face-attribute 'mode-line-inactive nil :underline  line)
;;		(set-face-attribute 'mode-line          nil :box        nil)
;;		(set-face-attribute 'mode-line-inactive nil :box        nil)
;;		(set-face-attribute 'mode-line-inactive nil :background "#f9f2d9")))

;; (use-package solarized-theme
;;   :config
;;   (load-theme 'solarized-light t)
;;   (let ((line (face-attribute 'mode-line :underline)))
;;     (set-face-attribute 'mode-line          nil :overline   line)
;;     (set-face-attribute 'mode-line-inactive nil :overline   line)
;;     (set-face-attribute 'mode-line-inactive nil :underline  line)
;;     (set-face-attribute 'mode-line          nil :box        nil)
;;     (set-face-attribute 'mode-line-inactive nil :box        nil)
;;     (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9")))


;; Use moody for the mode bar
(use-package moody
	:config
	(setq x-underline-at-descent-line t)
	(moody-replace-mode-line-buffer-identification)
	(moody-replace-vc-mode))

;; 隐藏minor modes
(use-package minions
  :config
  (setq minions-mode-line-lighter ""
		minions-mode-line-delimiters '("" . ""))
  (minions-mode 1))

;; 界面高亮配置
(global-hl-line-mode)

(use-package highlight-indent-guides
  :ensure t
  :diminish
  :hook (prog-mode . highlight-indent-guides-mode)
  :init (setq highlight-indent-guides-method 'column))

;; 高亮未提交代码
(use-package diff-hl
	:config
	(add-hook 'prog-mode-hook 'turn-on-diff-hl-mode))


;; 字体配置
(defun my-default-font()
  "Config my font."
  (interactive)
  (defvar my-fonts
	(cond ((eq system-type 'darwin) '("Monaco" "STHeiti"))
		  ((eq system-type 'gnu/linux) '("Noto Sans Mono" "Noto Sans CJK SC"))
		  ((eq system-type 'windows-nt) '("DejaVu Sans Mono" "Microsoft Yahei"))))
  (set-face-attribute 'default nil :font
					  (format "%s:pixelsize=%d" (car my-fonts) (if (> (nth 4 (assq 'geometry (car (display-monitor-attributes-list))))
																	  2000) 28 14)))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
	(set-fontset-font (frame-parameter nil 'font) charset
					  (font-spec :family (car (cdr my-fonts)) :size (if (> (nth 4 (assq 'geometry (car (display-monitor-attributes-list))))
																		   2000) 26 13))))
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


(use-package diminish
  :ensure t)


(use-package dired-subtree
  :defer t
  :bind (:map dired-mode-map
		  ("TAB" . dired-subtree-cycle)))

;; Dired
(setq dired-listing-switches "-AlShr")

(use-package avy
  :ensure t
  :bind
  ("C-=" . avy-goto-char)
  :config
  (setq avy-background t))

;; Evil
;;; evil config
(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (define-key evil-normal-state-map (kbd "s-T") 'vterm)
  (evil-mode 1))

(use-package evil-escape
  :ensure t
  :after evil ;; 表示在evil载入后才能载入
  :config
  (progn
	(setq-default evil-escape-key-sequence "kj")
	(setq-default evil-escape-delay 0.1)
	(evil-escape-mode t)
	))

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

(use-package evil-visualstar
		:config
		(global-evil-visualstar-mode)
		(setq evil-visualstar/persistent nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;项目配置
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :commands (projectile-mode projectile-switch-project)
  :bind
  (("C-c p f" . helm-projectile-find-file)
   ("C-c p p" . helm-projectile-switch-project)
   ("C-c p s" . projectile-save-project-buffers))
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Which Key
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

;; Custom keybinding
(use-package general
  :ensure t
  :config (general-define-key
		   :states '(normal visual insert emacs)
		   :prefix ","
		   :non-normal-prefix "C-,"
		   "/"   '(helm-projectile-rg :which-key "ripgrep")
		   "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
		   "SPC" '(helm-M-x :which-key "M-x")
		   "pf"  '(helm-projectile-find-file :which-key "find files")
		   "pp"  '(helm-projectile-switch-project :which-key "switch project")
		   "pb"  '(helm-projectile-switch-to-buffer :which-key "switch buffer")
		   "pr"  '(helm-show-kill-ring :which-key "show kill ring")
		   ;; Buffers
		   "bb"  '(helm-buffers-list :which-key "buffers list")
		   ;; Window
		   "wl"  '(windmove-right :which-key "move right")
		   "wh"  '(windmove-left :which-key "move left")
		   "wk"  '(windmove-up :which-key "move up")
		   "wj"  '(windmove-down :which-key "move bottom")
		   "w/"  '(split-window-right :which-key "split right")
		   "w-"  '(split-window-below :which-key "split bottom")
		   "wx"  '(delete-window :which-key "delete window")
		   ;; flycheck
		   "el" '(flycheck-list-errors :which-key "errors (flycheck)")
		   ;; magit
		   "gc" '(magit-commit :wk "git commit")
		   "gp" '(magit-push :wk "git push")
		   ;; Others
		   "vt"  '(vterm-other-window :which-key "open vterm window")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; helm
(use-package helm
  :ensure t
  :diminish helm-mode
  :bind (("C-x C-f" . helm-find-files)
		 ("M-x" . helm-M-x)
		 ("C-x b" . helm-mini)
		 ("C-x C-b" . helm-mini)
		 ("C-x C-o" . helm-occur)
		 ("M-y" . helm-show-kill-ring)
		 :map helm-map
		 ("<tab>" . helm-execute-persistent-action) ; Rebind TAB to expand
		 ("C-i" . helm-execute-persistent-action) ; Make TAB work in CLI
		 ("C-z" . helm-select-action)) ; List actions using C-z
  :init
  (require 'helm-config)
  :config
  (setq helm-buffers-fuzzy-matching t
		helm-recentf-fuzzy-match t
		helm-M-x-fuzzy-match t
		helm-split-window-inside-p t)
  (helm-mode))

(use-package helm-projectile
  :ensure t
  :after helm-mode
  :commands helm-projectile
  :bind ("C-c p h" . helm-projectile)
  :config
  (setq helm-projectile-fuzzy-match t))

(use-package helm-rg
  :bind
  (("C-c R" . helm-rg)))

;; Fancy titlebar for MacOS
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)


;; https://github.com/akermu/emacs-libvterm
(use-package vterm
  :load-path  "/home/tan/Projects/emacs-libvterm/"
  :config
  (add-hook 'vterm-mode-hook
			(lambda()
			  (setq-local evil-insert-state-cursor 'box)
			  (evil-insert-state)))
  (define-key vterm-mode-map [return] #'vterm-send-return)
  (setq vterm-keymap-exceptions nil)
  (evil-define-key 'insert vterm-mode-map (kbd "C-e")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-f")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-a")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-v")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-b")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-w")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-u")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-n")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-m")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-p")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-j")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-k")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-r")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-t")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-g")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-c")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-SPC")    #'vterm--self-insert)
  (evil-define-key 'normal vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
  (evil-define-key 'normal vterm-mode-map (kbd "i")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "o")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; kubernetes
(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))

;;; If you want to pull in the Evil compatibility package.
(use-package kubernetes-evil
  :ensure t
  :after kubernetes)

;;;;;;;;;;;;;;;;;;;; 开发相关配置

;; 括号提示
(use-package smartparens-config
  :ensure smartparens
  :diminish smartparens-mode
  :hook ((prog-mode . smartparens-mode)
		 (markdown-mode . smartparens-mode))
  :config
  (show-smartparens-global-mode t))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

;; 错误检查

;; Flycheck checks your code and helps show alerts from the linter
(use-package flycheck
  :ensure t
  :defer t
  :diminish flycheck-mode
  :hook (prog-mode . flycheck-mode)
  :config
  (progn
	(setq-default flycheck-phpcs-standard "PSR2")
	(setq-default flycheck-php-phpcs-executable "/bin/phpcs")))


;;; 版本管理

;;; Git
(use-package magit
  :ensure t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;补全和语法检查
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Programming Languages

;;; ############ company #################

;; company is the best autocompletion system for emacs (probably)
;; and this uses the language server to provide semantic completions
(use-package company
  :ensure t
  :commands (company-complete-common company-dabbrev)
  :bind
  (:map company-active-map
		("C-n" . company-select-next)
		("C-p" . company-select-previous)
		("<tab>" . company-complete-common))
  :hook (prog-mode . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-transformers nil)
  (company-tooltip-align-annotations t)
  (company-idle-delay 0.3)
  (company-tooltip-limit 10)
  ;; Trigger completion immediately.
  (company-idle-delay 0.1)
  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (company-show-numbers t)
  ;; Don't use company in the following modes
  (company-global-modes '(not shell-mode eaf-mode))
  :config
  (global-company-mode 1))

;; company-prescient: Simple but effective sorting and filtering for Emacs.
;; https://github.com/raxod502/prescient.el/tree/master
(use-package company-prescient
  :hook (company-mode . company-prescient-mode)
  :custom
  (prescient-persist-mode +1))


;;; ############ lsp #################

;; This is the main mode for LSP
;; https://github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
  :ensure t
  :defer t
  :commands (lsp-deferred)
  :custom
  (lsp-auto-guess-root nil)
  (lsp-prefer-flymake nil)
  (lsp-print-performance t)
  ;; for debugging, see `*lsp-log*' buffer
  (lsp-log-io t)
  (lsp-file-watch-threshold 2000)
  :bind (:map lsp-mode-map ("C-c C-f" . lsp-format-buffer))
  :hook ((python-mode go-mode) . lsp-deferred))

;; https://github.com/emacs-lsp/lsp-ui
(use-package lsp-ui
  :ensure t
  :diminish
  :commands lsp-ui-mode
  :after (lsp-mode flycheck)
  :bind (:map lsp-ui-mode-map
			  ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
			  ([remap xref-find-references] . lsp-ui-peek-find-references)
			  ("C-c u" . lsp-ui-imenu))
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil)
  (lsp-ui-flycheck-enable t)
  (lsp-ui-imenu-enable t)
  :config
  ;; Use lsp-ui-doc-webkit only in GUI
  (if *is-gui*
	  (setq lsp-ui-doc-use-webkit t)))

;; Lsp completion
;; https://github.com/tigersoldier/company-lsp
(use-package company-lsp
  :ensure t
  :requires company
  :defer t
  :commands company-lsp
  :config
  (push 'company-lsp company-backends)
  :custom
  (company-lsp-cache-candidates 'auto)
  (company-lsp-async t)
  (company-lsp-enable-snippet t))


;;; ############ Go #################
;;; [go-mode]: https://github.com/dominikh/go-mode.el

(use-package go-guru
  :defer)

;;; go get golang.org/x/tools/gopls@latest

(use-package go-mode
  :defer t
  :mode (("\\.go?\\'" . go-mode))
  :custom (gofmt-command "goimports")
  :bind (:map go-mode-map
			  ("C-c d" . lsp-describe-thing-at-point)
			  ("C-i" . company-indent-or-complete-common))
  :config
  (require 'go-guru)
  (add-hook 'go-mode-hook #'lsp)
  (add-hook 'go-mode-hook #'smartparens-mode)
  (add-hook 'before-save-hook #'lsp-format-buffer)
  (add-hook 'before-save-hook #'lsp-organize-imports))


;;; ############ Python #################
;;; 参考配置
;;; https://github.com/swaroopch/rangoli-emacs/blob/master/features/rangoli-python.el
;;; https://github.com/CSRaghunandan/.emacs.d/blob/master/setup-files/setup-python.el
;;; https://github.com/luismayta/emacs.d/blob/develop/src/modules/module-coding-python.el

(use-package python
  :ensure t
  :mode (("\\.py\\'" . python-mode))
  :init
  (setq python-indent-offset 4))

(use-package auto-virtualenv
  :ensure t
  :config
  (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
  (add-hook 'projectile-after-switch-project-hook 'auto-virtualenv-set-virtualenv)
  (add-hook 'pyvenv-post-activate-hooks 'pyvenv-restart-python))

(use-package python-environment
  :ensure t)

(use-package pyvenv
  :ensure t)

(defvar tyw/pyenv-root (f-expand "~/.pyenv/"))

;;; pyenv 相关配置,主要解决自身项目开发环境问题和Emacs的python路径问题
(use-package pyenv-mode
  :ensure t
  :hook ((python-mode . pyenv-mode))
  :init
  (setenv "WORKON_HOME" (f-join tyw/pyenv-root "versions"))
  (add-to-list 'exec-path (f-join tyw/pyenv-root "shims"))
  :config
  (setq python-shell-interpreter (f-join tyw/pyenv-root "shims/python")
		flycheck-python-pycompile-executable  (f-join tyw/pyenv-root "shims/python")
		flycheck-python-flake8-executable (f-join tyw/pyenv-root "shims/python")
		flycheck-python-pylint-executable (f-join tyw/pyenv-root "shims/python")
		flycheck-python-mypy-executable (f-join tyw/pyenv-root "shims/mypy")
		lsp-python-ms-python-executable-cmd (f-join tyw/pyenv-root "shims/python")
		treemacs-python-executable (f-join tyw/pyenv-root "shims/python")
		lsp-pyls-server-command (f-join tyw/pyenv-root "shims/pyls"))
  (add-to-list 'python-shell-exec-path (f-join tyw/pyenv-root "shims"))
  :bind
  ("C-x p e" . pyenv-activate-current-project))

(use-package pyenv-mode-auto
  :config
  (add-hook 'pyenv-mode-auto-hook
	(lambda () (shell-command "pip install autopep8 flake8 isort yapf pylint")))
  )

(defun pyenv-init()
  (setq global-pyenv (replace-regexp-in-string "\n" "" (shell-command-to-string "pyenv global")))
  (message (concat "Setting pyenv version to " global-pyenv))
  (pyenv-mode-set global-pyenv)
  (defvar pyenv-current-version nil global-pyenv))

(defun pyenv-activate-current-project()
  "Automatically activates pyenv version if .python-version file exists."
  (interactive)
  (f-traverse-upwards
   (lambda (path)
	 (let ((pyenv-version-path (f-expand ".python-version" path)))
	   (if (f-exists? pyenv-version-path)
	  (progn
		(setq pyenv-current-version (s-trim (f-read-text pyenv-version-path 'utf-8)))
		(pyenv-mode-set pyenv-current-version)
		(pyvenv-workon pyenv-current-version)
		(message (concat "Setting virtualenv to " pyenv-current-version))))))))

(add-hook 'after-init-hook 'pyenv-init)
(add-hook 'projectile-after-switch-project-hook 'pyenv-activate-current-project)

(defun python-select-interpreter (path)
  "Select current python interpreter for all related services (Flycheck, Eshell, etc.)."
  (interactive "FPath: ")

  (setq python-shell-interpreter             path
		flycheck-python-pylint-executable    path
		flycheck-python-pycompile-executable path
		flycheck-python-flake8-executable    path
		lsp-python-ms-python-executable-cmd  path
		treemacs-python-executable           path))

(use-package lsp-python-ms
  :ensure t
  :defer t
  :hook (python-mode . (lambda ()
						 (pyenv-activate-current-project)
						 (require 'lsp-python-ms)
						 (lsp)))
  :config
  ;; for dev build of language server
  (setq lsp-python-ms-executable
		"/home/tan/Projects/python-language-server/output/bin/Release/linux-x64/publish/Microsoft.Python.LanguageServer"))

;; pip-requirements: Major mode for editing pip requirements files
;; https://github.com/Wilfred/pip-requirements.el
(use-package pip-requirements
  :hook ((pip-requirements-mode . company-mode))
  :init
  (setq pip-requirements-index-url "https://mirrors.cloud.tencent.com/pypi/simple/"))

(use-package py-isort
  :ensure t
  :config
  (add-hook 'before-save-hook 'py-isort-before-save))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; snippets
(use-package yasnippet                  ; Snippets
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode)
  :config
  (setq yas-verbosity 1)                      ; No need to be so verbose
  (setq yas-wrap-around-region t)
  (setq yas-snippet-dirs (append yas-snippet-dirs
								 '("~/.emacs.d/snippets")))
  (yas-reload-all))

(use-package yasnippet-snippets         ; Collection of snippets
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; yaml-mode
(use-package yaml-mode
  :ensure t
  :mode
  (("\\.yml$" . yaml-mode)
   ("\\.yaml$" . yaml-mode))
  :config
  (use-package ansible
	:config
	(use-package company-ansible)
	(add-to-list 'company-backends 'company-ansible)
	(add-hook 'ansible-hook 'ansible::auto-decrypt-encrypt))
  (add-hook 'yaml-mode-hook '(lambda () (ansible 1))))

;;; jinja2-mode
(use-package jinja2-mode
	:mode ("\\.j2\\'" . jinja2-mode)
	:defer t)

;;; json-mode
(use-package json-mode
  :mode ("\\.json\\'" . json-mode))

;;; jsonnet-mode
(use-package jsonnet-mode
  :mode ("\\.jsonnet\\'" . jsonnet-mode))


;;; lua environment
(use-package lua-mode
  :ensure t
  :mode ("\\.lua$" . lua-mode)
  :config
  (setq lua-indent-level 4))

;; PHP environment
(use-package php-mode
  :config
  (progn
	(setq-default php-mode-coding-style 'psr2)))


;;; Dockerfile
(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

;; emacs mode for editing ssh config files.
;; https://github.com/jhgorrell/ssh-config-mode-el
(use-package ssh-config-mode
  :ensure t
  :defer t)

;; Org Mode
(use-package htmlize
  :ensure t)

;; hugo blog
(use-package ox-hugo
  :ensure t            ;Auto-install the package from Melpa (optional)
  :after ox)


(use-package org
  :defines org-publish-project-alist
  :functions org-publish-find-date org-publish-sitemap-default-entry
  :config
  ;;; orgmode 下源码高亮
  (setq org-src-fontify-natively t)
  (setq org-ellipsis "⤵")
  (setq org-src-tab-acts-natively t)
  (defun rk/org-publish-sitemap-time-entry (entry style project)
	"My org sitemap entry with time ENTRY STYLE PROJECT rk short for 9r0k."
	(format "%s %s"
		(format-time-string
		 "[%Y-%m-%d]"
		 (org-publish-find-date entry project))
		(org-publish-sitemap-default-entry entry style project)))
  (setq org-publish-project-alist
	'(
	  ("blog"
	   :components ("blog-content" "blog-static"))
	  ("blog-content"
	   :base-directory "~/Projects/OrgNote/Blog"
	   :base-extension "org" ;扩展名
	   :publishing-directory "~/Projects/Rand01ph.github.io"
	   :recursive t
	   :publishing-function (org-html-publish-to-html)
	   :headline-levels 4
	   :section-numbers nil
	   :with-toc t

	   :auto-sitemap t                ; Generate sitemap.org automagically...
	   :sitemap-filename "index.org"  ; ... call it sitemap.org (it's the default)...
	   :sitemap-title "Just for fun"         ; ... with title 'Sitemap'.
	   :sitemap-sort-files anti-chronologically
	   :sitemap-format-entry rk/org-publish-sitemap-time-entry

	   :html-doctype "html5"
	   :html-validation-link nil
	   :html-preamble: nil
	   :html-head-include-scripts nil ;Disable the default javascript snippet
	   :html-head-include-default-style nil ;Disable the default css style
	   :html-head "
<link rel=\"stylesheet\" type=\"text/css\" href=\"https://gongzhitaao.org/orgcss/org.css\"/>
<!-- Global site tag (gtag.js) - Google Analytics -->
<script async src=\"https://www.googletagmanager.com/gtag/js?id=UA-67269379-3\"></script>
<script>
  window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag('js', new Date());

  gtag('config', 'UA-67269379-3');
</script>

<a href=\"https://github.com/Rand01ph\" class=\"github-corner\" aria-label=\"Follow me on GitHub\"><svg width=\"80\" height=\"80\" viewBox=\"0 0 250 250\" style=\"fill:#151513; color:#fff; position: absolute; top: 0; border: 0; left: 0; transform: scale(-1, 1);\" aria-hidden=\"true\"><path d=\"M0,0 L115,115 L130,115 L142,142 L250,250 L250,0 Z\"></path><path d=\"M128.3,109.0 C113.8,99.7 119.0,89.6 119.0,89.6 C122.0,82.7 120.5,78.6 120.5,78.6 C119.2,72.0 123.4,76.3 123.4,76.3 C127.3,80.9 125.5,87.3 125.5,87.3 C122.9,97.6 130.6,101.9 134.4,103.2\" fill=\"currentColor\" style=\"transform-origin: 130px 106px;\" class=\"octo-arm\"></path><path d=\"M115.0,115.0 C114.9,115.1 118.7,116.5 119.8,115.4 L133.7,101.6 C136.9,99.2 139.9,98.4 142.2,98.6 C133.8,88.0 127.5,74.4 143.8,58.0 C148.5,53.4 154.0,51.2 159.7,51.0 C160.3,49.4 163.2,43.6 171.4,40.1 C171.4,40.1 176.1,42.5 178.8,56.2 C183.1,58.6 187.2,61.8 190.9,65.4 C194.5,69.0 197.7,73.2 200.1,77.6 C213.8,80.2 216.3,84.9 216.3,84.9 C212.7,93.1 206.9,96.0 205.4,96.6 C205.1,102.4 203.0,107.8 198.3,112.5 C181.9,128.9 168.3,122.5 157.7,114.1 C157.9,116.9 156.7,120.9 152.7,124.9 L141.0,136.5 C139.8,137.7 141.6,141.9 141.8,141.8 Z\" fill=\"currentColor\" class=\"octo-body\"></path></svg><style>.github-corner:hover .octo-arm{animation:octocat-wave 560ms ease-in-out}@keyframes octocat-wave{0%,100%{transform:rotate(0)}20%,60%{transform:rotate(-25deg)}40%,80%{transform:rotate(10deg)}}@media (max-width:500px){.github-corner:hover .octo-arm{animation:none}.github-corner .octo-arm{animation:octocat-wave 560ms ease-in-out}}</style></a>"

	   :html-link-home "index.html"
	   :html-link-up "index.html"

	   :author "Rand01ph"
	   :email "tanyawei1991@gmail.com"
	   :language "zh-CN")

	  ("blog-static"
	   :base-directory "~/Projects/OrgNote/Blog/static"
	   :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf"
	   :publishing-directory "~/Projects/Rand01ph.github.io/static/"
	   :recursive t
	   :publishing-function (org-publish-attachment)))))

(use-package org-bullets
	:init
	(add-hook 'org-mode-hook 'org-bullets-mode))

(org-babel-do-load-languages
 'org-babel-load-languages '((C . t)
							 (python . t)
							 (emacs-lisp . t)
							 (shell . t)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
	("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" default)))
 '(lsp-log-io t t)
 '(lsp-print-performance nil t)
 '(lsp-trace nil t)
 '(package-selected-packages
   (quote
	(ox-hugo py-isort pyenv-mode-auto python-environment auto-virtualenv pip-requirements ssh-config-mode dockerfile-mode company-prescient lsp-ui go-guru jsonnet-mode go-tag gotest neotree general moody evil-visualstar monokai-theme dired-subtree highlight-indent-guides request ms-python company-box yasnippet-snippets yaml-mode which-key web-mode use-package treemacs ssh-deploy solarized-theme smartparens smart-mode-line-powerline-theme shrink-path rainbow-delimiters pyvenv python-mode pyenv-mode prettier-js phpcbf php-mode moe-theme lua-mode lsp-python-ms kubernetes-tramp kubernetes-evil json-mode js2-refactor htmlize helm-rg helm-projectile go-mode flycheck exec-path-from-shell evil-surround evil-nerd-commenter evil-leader evil-escape emmet-mode eldoc-eval dracula-theme doom-themes diminish company-lsp auto-package-update anzu)))
 '(safe-local-variable-values (quote ((encoding . utf-8)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-ui-doc-background ((t (:background nil)))))
