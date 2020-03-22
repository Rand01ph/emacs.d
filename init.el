;;; init.el --- Rand01ph's Emacs configurations.  -*- lexical-binding: t; no-byte-compile: nil; -*-

;; Version: 0.0.5

;;; Commentary:
;; Emacs 配置,缓慢进化中
;; init configurations.
;;


;; 外观配置
;; Minimal UI
;;; Disable toolbar & menubar & scroll-bar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tooltip-mode) (tooltip-mode -1))
(set-window-scroll-bars (minibuffer-window) nil nil)


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

;; for faster emacs start-up; gets re-set later
(setq gc-cons-threshold (* 50 1000 1000))

;; set load path
(add-to-list 'load-path (concat user-emacs-directory "elisp"))

;;; Basic Setup
(setq user-full-name "Rand01ph"
	  user-mail-address "tanyawei1991@gmail.com")

;;; Bindings
;; Unbind unneeded keys
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "M-z") nil)
(global-set-key (kbd "C-x C-z") nil)
(global-set-key (kbd "M-/") nil)
;; Truncate lines
(global-set-key (kbd "C-x C-l") #'toggle-truncate-lines)

;;; 编码配置
;; Use UTF8 everywhere, see https://thraxys.wordpress.com/2016/01/13/utf-8-in-emacs-everywhere-forever/
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
   (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

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
;; If you're using Emacs on macOS, you can add this to have your titlebar color changed and matching your color theme:
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 核心模块
;; files and directories
(use-package f)

(use-package memoize)

(use-package all-the-icons)

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
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("PATH" "GOPATH" "PYTHONPATH")))


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
;;   :config
;;   (load-theme 'doom-one t)
;;   (doom-themes-visual-bell-config))

(use-package monokai-theme
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


(use-package diminish)


(use-package dired-subtree
  :bind (:map dired-mode-map
		  ("TAB" . dired-subtree-cycle)))

;; Dired
(setq dired-listing-switches "-AlShr")

(use-package avy
  :bind
  ("C-=" . avy-goto-char)
  :config
  (setq avy-background t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Evil
;;; evil config
(use-package evil
  :init
  ;(setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1))

(use-package evil-escape
  :config
  (progn
	(setq-default evil-escape-key-sequence "kj")
	(setq-default evil-escape-delay 0.1)
	(evil-escape-mode t)
	))

(use-package evil-surround
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
  :commands (projectile-mode projectile-switch-project)
  :bind
  (("C-c p f" . helm-projectile-find-file)
   ("C-c p p" . helm-projectile-switch-project)
   ("C-c p s" . projectile-save-project-buffers))
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; keybind

;; Which Key
(use-package which-key
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

;; Custom keybinding
(use-package general
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
  (setq helm-M-x-fuzzy-match t
		helm-mode-fuzzy-match t
		helm-buffers-fuzzy-matching t
		helm-recentf-fuzzy-match t
		helm-locate-fuzzy-match t
		helm-semantic-fuzzy-match t
		helm-imenu-fuzzy-match t
		helm-completion-in-region-fuzzy-match t
		helm-candidate-number-list 150
		helm-split-window-in-side-p t)
  (helm-mode 1))

(use-package helm-projectile
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
  :commands (kubernetes-overview))

;;; If you want to pull in the Evil compatibility package.
(use-package kubernetes-evil
  :after kubernetes)

;;;;;;;;;;;;;;;;;;;; 开发相关配置

;; format all
(use-package format-all
  :bind ("C-z C-f" . format-all-buffer))


;; 括号提示
(use-package smartparens
  :diminish smartparens-mode
  :hook ((prog-mode . smartparens-mode)
		 (markdown-mode . smartparens-mode))
  :config
  (show-smartparens-global-mode t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; 错误检查

;; Flycheck checks your code and helps show alerts from the linter
(use-package flycheck
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
  :hook (with-editor-mode . evil-insert-state))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;补全和语法检查
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Programming Languages

;;; ############ company #################

;; company is the best autocompletion system for emacs (probably)
;; and this uses the language server to provide semantic completions
(use-package company
  :commands (company-complete-common company-dabbrev)
  :bind
  (:map company-active-map
		("C-n" . company-select-next)
		("C-p" . company-select-previous)
		("<tab>" . company-complete-common))
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
  (company-global-modes '(not erc-mode message-mode help-mode gud-mode eshell-mode shell-mode))
  :config
  (global-company-mode +1))

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
  :init
  ;; @see https://github.com/emacs-lsp/lsp-mode#performance
  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  :hook ((lsp-after-open . lsp-enable-imenu)
		 (lsp-mode . lsp-enable-which-key-integration))
  :bind (:map lsp-mode-map
			  ("C-c C-d" . lsp-describe-thing-at-point)
			  ("C-c C-f" . lsp-format-buffer))
  :custom
  (lsp-prefer-flymake nil) ; Use flycheck instead of flymake
  (lsp-print-performance t)
  ;; for debugging, see `*lsp-log*' buffer
  (lsp-log-io t)
  (lsp-file-watch-threshold 2000)
  (read-process-output-max (* 1024 1024))
  :config
  (evil-define-key 'normal lsp-mode-map
	  (kbd "g r")        'lsp-find-references
	  (kbd "g d")        'lsp-find-definition
	)
  (setq lsp-auto-guess-root t ; Detect project root
		lsp-keep-workspace-alive nil)) ; Auto-kill LSP server

;; https://github.com/emacs-lsp/lsp-ui
(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :bind (:map lsp-ui-mode-map
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
  :requires company
  :commands company-lsp
  :config
  (push 'company-lsp company-backends)
  :custom
  (company-lsp-cache-candidates 'auto)
  (company-lsp-async t)
  (company-lsp-enable-snippet t))


;;; ############ Go #################
;;; [go-mode]: https://github.com/dominikh/go-mode.el

(defun my-try-go-mod (dir)
  "Find go project root for DIR."
  (if (and dir
		   (not (f-descendant-of-p dir (or (getenv "GOPATH")
										   (concat (getenv "HOME") "/go")))))
	  (let ((result (locate-dominating-file dir "go.mod")))
		(if result
			(cons 'transient (expand-file-name result))
		  (cons 'transient dir)))
	(when dir
		(cons 'transient dir))))

(defun my-go-project-setup ()
  "Set project root for go project."
  (setq-local project-find-functions (list #'my-try-go-mod #'project-try-vc)))

(use-package go-guru)

;;; go get golang.org/x/tools/gopls@latest

(use-package go-mode
  :mode (("\\.go?\\'" . go-mode))
  :custom (gofmt-command "goimports")
  :bind (:map go-mode-map
			  ("C-c d" . lsp-describe-thing-at-point)
			  ("C-i" . company-indent-or-complete-common))
  :hook ((go-mode . my-go-project-setup))
  :config
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package go-playground)

;;; ############ Python #################
;;; 参考配置

(use-package python
  :mode (("\\.py\\'" . python-mode))
  :custom
  (python-indent-offset 4)
  (python-indent-guess-indent-offset nil))

  ;; (use-package pyenv
  ;;	:straight (:host github :repo "aiguofer/pyenv.el")
  ;;	:init
  ;;	(setq pyenv-installation-dir "/usr/local")
  ;;	:config
  ;;	;;(setq pyenv-use-alias 't)
  ;;	(setq pyenv-modestring-prefix " ")
  ;;	(setq pyenv-modestring-postfix nil)
  ;;	;; this will remove the colors
  ;;	(setq pyenv-modeline-function 'pyenv--modeline-plain)
  ;;	(setq pyenv-set-path t)

  ;;	(global-pyenv-mode)
  ;;	(defun pyenv-update-on-buffer-switch (prev curr)
  ;;	  (if (string-equal "Python" (format-mode-line mode-name nil nil curr))
  ;;		  (pyenv-use-corresponding)))
  ;;	(add-hook 'switch-buffer-functions 'pyenv-update-on-buffer-switch))

(use-package pyvenv
  :config
  (setenv "WORKON_HOME" "~/.pyenv/versions"))

(use-package conda
  :custom
  (conda-anaconda-home "/usr/local/Caskroom/miniconda/base/"))

(use-package lsp-python-ms
  :hook (python-mode . (lambda ()
						 (lsp)))
  :config
  (setq lsp-python-ms-extra-paths "/Users/tanyawei/.pyenv/versions/Hato/lib/python2.7/site-packages")
  ;; for dev build of language server
  (setq lsp-python-ms-executable
		"~/Projects/python-language-server/output/bin/Release/osx-x64/publish/Microsoft.Python.LanguageServer"))

;; pip-requirements: Major mode for editing pip requirements files
;; https://github.com/Wilfred/pip-requirements.el
;; (use-package pip-requirements
;;   :hook ((pip-requirements-mode . company-mode))
;;   :init
;;   (setq pip-requirements-index-url "https://mirrors.cloud.tencent.com/pypi/simple/"))

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
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode)
  :config
  (setq yas-verbosity 1)                      ; No need to be so verbose
  (setq yas-wrap-around-region t)
  (setq yas-snippet-dirs (append yas-snippet-dirs
								 '("~/.emacs.d/snippets")))
  (yas-reload-all))

(use-package yasnippet-snippets)         ; Collection of snippets

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; yaml-mode
(use-package yaml-mode
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
	:mode ("\\.j2\\'" . jinja2-mode))

;;; json-mode
(use-package json-mode
  :mode ("\\.json\\'" . json-mode))

;;; jsonnet-mode
(use-package jsonnet-mode
  :mode ("\\.jsonnet\\'" . jsonnet-mode))


;;; lua environment
(use-package lua-mode
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
(use-package ssh-config-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Org Mode
(use-package org
  :defines org-publish-project-alist
  :functions org-publish-find-date org-publish-sitemap-default-entry
  :config
  ;;; orgmode 下源码高亮
  (setq org-src-fontify-natively t)
  (setq org-ellipsis "⤵")
  (setq org-src-tab-acts-natively t)
  (add-to-list 'org-modules 'org-tempo))

;; 开启org自动换行
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

(use-package htmlize)

;; hugo blog
(use-package ox-hugo
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
