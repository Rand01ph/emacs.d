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

;;; Bindings
;; Unbind unneeded keys
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "M-z") nil)
(global-set-key (kbd "C-x C-z") nil)
(global-set-key (kbd "M-/") nil)
;; Truncate lines
(global-set-key (kbd "C-x C-l") #'toggle-truncate-lines)

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

;; 常量配置
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MacOS 配置
(when (eq system-type 'darwin)
  ;; Assign command key to meta
  (setq mac-command-modifier 'meta
		;; Disabled option key
		mac-option-modifier nil
		;; Better scrolling on Mac
		mac-redisplay-dont-reset-vscroll t
		mac-mouse-wheel-smooth-scroll nil
		;; Native fullscreen sucks on Mac
		ns-use-native-fullscreen nil
		;; Don't open files from the workspace in a new frame
		ns-pop-up-frames nil))

;; Fancy titlebar for MacOS
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 核心模块
;; files and directories
(use-package f
  :ensure t)

(use-package memoize
  :ensure t)

(use-package all-the-icons;
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
																	  2000) 26 14)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Evil
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
	(setq-default evil-escape-delay 0.1)
	(evil-escape-mode t)
	))

(use-package evil-surround
  :ensure t
  :hook ((text-mode prog-mode wdired-mode) . evil-surround-mode))

(use-package evil-nerd-commenter
  :commands (evilnc-quick-comment-or-uncomment-to-the-line
			 evilnc-copy-and-comment-lines
			 evilnc-comment-or-uncomment-paragraphs)
  :bind (([remap comment-dwim] . evilnc-comment-or-uncomment-lines)) ;; M-;
  :init
  (bind-keys :prefix-map evilnc-prefix-map
			 :prefix "C-c c"
			 ("l" . evilnc-quick-comment-or-uncomment-to-the-line)
			 ("c" . evilnc-copy-and-comment-lines)
			 ("p" . evilnc-comment-or-uncomment-paragraphs)))

(use-package evil-visualstar
		:config
		(global-evil-visualstar-mode)
		(setq evil-visualstar/persistent nil))

(use-package evil-magit
  :requires evil
  :after magit
  :hook (magit-mode . evil-magit-init))

(use-package evil-org
  :requires evil
  :after org
  :diminish
  :hook ((org-mode . evil-org-mode)
		 (org-agenda-mode . evil-org-mode)
		 (evil-org-mode . (lambda () (evil-org-set-key-theme)
							(require 'evil-org-agenda)
							(evil-org-agenda-set-keys)))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; keybind

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
		   :prefix "SPC"
		   :non-normal-prefix "M-SPC"
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


;; https://github.com/akermu/emacs-libvterm
(use-package vterm
  :load-path  "~/Projects/emacs-libvterm"
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

;; format all
(use-package format-all
  :bind ("C-z C-f" . format-all-buffer))


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
  :init (setq flycheck-keymap-prefix (kbd "C-c f"))
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  ;; Disable the default keybindings here
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map)
  (progn
	(setq-default flycheck-phpcs-standard "PSR2")
	(setq-default flycheck-php-phpcs-executable "/bin/phpcs")))


;;; 版本管理

;;; Git
(use-package magit
  :ensure t
  :hook (with-editor-mode . evil-insert-state))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;补全和语法检查
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Programming Languages

;;; ############ company #################

;; company is the best autocompletion system for emacs (probably)
;; and this uses the language server to provide semantic completions
(use-package company
  :ensure t
  :diminish
  :commands (company-complete-common company-dabbrev)
  :bind
  (:map company-active-map
		("C-n" . company-select-next)
		("C-p" . company-select-previous)
		("<tab>" . company-complete-common))
 :hook ((prog-mode
		  comint-mode
		  text-mode) . company-mode)
  :custom
  (company-minimum-prefix-length 2)
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
  :defer t
  :commands lsp
  :custom
  (lsp-auto-guess-root nil)
  (lsp-prefer-flymake nil) ; Use flycheck instead of flymake
  (lsp-print-performance t)
  ;; for debugging, see `*lsp-log*' buffer
  (lsp-log-io t)
  (lsp-file-watch-threshold 2000)
  (read-process-output-max (* 1024 1024))
  :bind (:map lsp-mode-map ("C-c C-f" . lsp-format-buffer))
  :hook ((python-mode go-mode
		  js-mode js2-mode typescript-mode web-mode
		  c-mode c++-mode objc-mode) . lsp))

;; https://github.com/emacs-lsp/lsp-ui
(use-package lsp-ui
  :after lsp-mode
  :diminish
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :bind (:map lsp-ui-mode-map
			  ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
			  ([remap xref-find-references] . lsp-ui-peek-find-references)
			  ("C-c u" . lsp-ui-imenu)
			  ("M-i" . lsp-ui-doc-focus-frame))
  :custom
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil)
  :config
  ;; Use lsp-ui-doc-webkit only in GUI
  (if *is-gui*
	  (setq lsp-ui-doc-use-webkit t))
  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
	(setq mode-line-format nil))
  ;; Waiting for https://github.com/emacs-lsp/lsp-ui/pull/390
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide))

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

(use-package go-playground
  :defer t)

;;; ############ Python #################
;;; 参考配置
;;; https://github.com/swaroopch/rangoli-emacs/blob/master/features/rangoli-python.el
;;; https://github.com/CSRaghunandan/.emacs.d/blob/master/setup-files/setup-python.el
;;; https://github.com/luismayta/emacs.d/blob/develop/src/modules/module-coding-python.el

(use-package python
  :ensure nil
  :after flycheck
  :mode (("\\.py\\'" . python-mode))
  :custom
  (python-indent-offset 4)
  (flycheck-python-pycompile-executable "python3")
  (python-shell-interpreter "python3"))

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
		;;treemacs-python-executable (f-join tyw/pyenv-root "shims/python")
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
  :after lsp-mode python
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Web Development
;;; npm i -g typescript
;;; npm i -g typescript-language-server

;;; Web Mode
(use-package web-mode
  :custom-face
  (css-selector ((t (:inherit default :foreground "#66CCFF"))))
  (font-lock-comment-face ((t (:foreground "#828282"))))
  :mode
  ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'"
   "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.[t]?html?\\'"))

;;; JavaScript/TypeScript
(use-package js2-mode
  :mode "\\.js\\'"
  :interpreter "node")

(use-package typescript-mode
  :mode "\\.ts\\'"
  :commands (typescript-mode))

(use-package emmet-mode
  :hook ((web-mode . emmet-mode)
		 (css-mode . emmet-mode)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Org Mode
(use-package org
  :ensure nil
  :defer t
  :defines org-publish-project-alist
  :functions org-publish-find-date org-publish-sitemap-default-entry
  :config
  ;;; orgmode 下源码高亮
  (setq org-src-fontify-natively t)
  (setq org-ellipsis "⤵")
  (setq org-src-tab-acts-natively t)
  (unless (version< org-version "9.2")
	(require 'org-tempo)))

;; 开启org自动换行
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

(use-package htmlize
  :ensure t)

;; hugo blog
(use-package ox-hugo
  :ensure t            ;Auto-install the package from Melpa (optional)
  :after ox)


(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(org-babel-do-load-languages
 'org-babel-load-languages '((C . t)
							 (python . t)
							 (emacs-lisp . t)
							 (js . t)
							 (shell . t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
	(company-ansible ansible emmet-mode typescript-mode js2-mode web-mode format-all evil-org evil-magit all-the-icons memoize yasnippet-snippets yaml-mode which-key use-package ssh-deploy ssh-config-mode smartparens smart-mode-line-powerline-theme rainbow-delimiters python-environment pyenv-mode-auto py-isort pip-requirements php-mode ox-hugo org-bullets moody monokai-theme minions lua-mode lsp-ui lsp-python-ms kubernetes-tramp kubernetes-evil jsonnet-mode json-mode jinja2-mode hydra htmlize highlight-indent-guides helm-rg helm-projectile go-playground go-guru general flycheck exec-path-from-shell evil-visualstar evil-surround evil-nerd-commenter evil-leader evil-escape doom-themes dockerfile-mode dired-subtree diminish diff-hl company-prescient company-lsp avy auto-virtualenv auto-package-update anzu))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(css-selector ((t (:inherit default :foreground "#66CCFF"))))
 '(font-lock-comment-face ((t (:foreground "#828282"))))
 '(lsp-ui-doc-background ((t (:background nil))))
 '(lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic))))))
