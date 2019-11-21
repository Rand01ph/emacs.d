;;; init.el --- Rand01ph's Emacs configurations.  -*- lexical-binding: t; no-byte-compile: nil; -*-

;; Version: 0.0.2

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
      '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
	("melpa" . "http://elpa.emacs-china.org/melpa/")
	("org"   . "http://elpa.emacs-china.org/org/")))
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

;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   (load-theme 'doom-one t)
;;   (doom-themes-visual-bell-config))
(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t))

(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/theme 'dark)
  (add-hook 'after-init-hook 'sml/setup))

;; highlight
(use-package highlight-indent-guides
  :ensure t
  :diminish
  :hook (prog-mode . highlight-indent-guides-mode)
  :init (setq highlight-indent-guides-method 'character))

;; 字体配置
;; M+ 1m    Noto Sans CJK SC
(defun my-default-font()
  "Config my font."
  (interactive)
  (defvar my-fonts
    (cond ((eq system-type 'darwin)     '("Monaco"           "STHeiti"))
	  ((eq system-type 'gnu/linux)  '("M+ 1m"            "Noto Sans CJK SC"))
	  ((eq system-type 'windows-nt) '("DejaVu Sans Mono" "Microsoft Yahei"))))
  (set-face-attribute 'default nil :font
		      (format "%s:pixelsize=%d" (car my-fonts) 14))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
		      (font-spec :family (car (cdr my-fonts)) :size 12)))
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

;; 括号提示
(use-package smartparens-config
  :ensure smartparens
  :diminish smartparens-mode
  :config
  (show-smartparens-global-mode t))

(add-hook 'prog-mode-hook 'turn-on-smartparens-mode)
(add-hook 'markdown-mode-hook 'turn-on-smartparens-mode)

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode +1))

(use-package dired-subtree
  :defer t
  :bind (:map dired-mode-map
	      ("TAB" . dired-subtree-cycle)))

(use-package avy
  :ensure t
  :bind
  ("C-=" . avy-goto-char)
  :config
  (setq avy-background t))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;项目配置
(use-package projectile
  :ensure t
  :diminish projectile-mode
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; helm
(use-package helm
  :diminish helm-mode
  :ensure t
  :bind (("C-x C-f" . helm-find-files)
	 ("M-x" . helm-M-x)
	 ("C-x b" . helm-mini)
	 ("C-x C-b" . helm-mini)
	 ("M-y" . helm-show-kill-ring)
	 :map helm-map
	 ("<tab>" . helm-execute-persistent-action) ; Rebind TAB to expand
	 ("C-i" . helm-execute-persistent-action) ; Make TAB work in CLI
	 ("C-z" . helm-select-action)) ; List actions using C-z
  :config
  (progn
    (setq-default helm-split-window-in-side-p t)
    (helm-mode 1)))

(use-package helm-projectile
  :ensure t
  :bind (("C-x , p" . helm-projectile-switch-project)
	 ("C-x , f" . helm-projectile-find-file)
	 ("C-x , b" . projectile-ibuffer)
	 ("C-x , i" . projectile-invalidate-cache)
	 ("C-x , a" . helm-projectile-ag))
  :config
  (progn
    (use-package helm-rg
      :ensure t
      :bind (("C-c k" . helm-rg)))

    (projectile-mode)
    (setq-default projectile-enable-caching t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; kubernetes
(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))

(use-package kubernetes-tramp
  :ensure t)

;; If you want to pull in the Evil compatibility package.
;; (use-package kubernetes-evil
;;   :ensure t
;;   :after kubernetes)

(use-package hydra
  :ensure t)

(use-package ssh-deploy
    :ensure t
    :demand
    :after hydra
    :config
    (ssh-deploy-line-mode) ;; If you want mode-line feature
    (ssh-deploy-add-menu) ;; If you want menu-bar feature
    (ssh-deploy-hydra "C-c C-z") ;; If you want the hydra feature
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;补全和语法检查
;; This is the main mode for LSP
(use-package lsp-mode
  ;; :diminish lsp-mode
  :defer t
  ;; :hook (prog-mode . lsp)
  :hook (python-mode . lsp-deferred)
	(go-mode . lsp-deferred)
  :bind (:map lsp-mode-map
	      ("C-c C-d" . lsp-describe-thing-at-point))
  :init
  (setq lsp-auto-guess-root t)       ; Detect project root
  ;; disable Yasnippet
  (setq lsp-enable-snippet nil)
  (setq lsp-prefer-flymake nil)      ; Use lsp-ui and flycheck
  (setq flymake-fringe-indicator-position 'right-fringe)
)

(use-package lsp-ui
  :defer t
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  :bind (:map lsp-ui-mode-map
	      ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
	      ([remap xref-find-references] . lsp-ui-peek-find-references)
	      ("C-c u" . lsp-ui-imenu))
  :init (setq lsp-ui-doc-enable t
	      lsp-ui-doc-use-webkit t
	      lsp-ui-doc-include-signature t
	      lsp-ui-doc-position 'top
	      lsp-ui-doc-border (face-foreground 'default)

	      ;; lsp-enable-snippet nil
	      lsp-ui-sideline-enable nil
	      ;; emacs26.2 经常陷入卡顿, set it to nil.
	      lsp-use-native-json nil
	      lsp-ui-sideline-ignore-duplicate t)
  :config
  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil)))

(use-package company-lsp
  :defer t
  :init (setq company-lsp-cache-candidates 'auto))

;; company is the best autocompletion system for emacs (probably)
;; and this uses the language server to provide semantic completions
(use-package company
  :commands (company-complete-common company-dabbrev)
  :config
  (global-company-mode)
  (setq company-minimum-prefix-length 1
	    company-idle-delay 0
	    company-tooltip-limit 10
	    company-transformers nil
	    company-show-numbers t
	    )
  (push 'company-lsp company-backends)
  ;; Keymap: hold Ctrl for Vim motion. Why?
  ;; .. we're already holding Ctrl, allow navigation at the same time.
  (define-key company-active-map (kbd "C-j") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-k") 'company-select-previous-or-abort)
  (define-key company-active-map (kbd "C-l") 'company-complete-selection)
  (define-key company-active-map (kbd "C-h") 'company-abort)
  (define-key company-active-map (kbd "<C-return>") 'company-complete-selection)

  (define-key company-search-map (kbd "C-j") 'company-select-next)
  (define-key company-search-map (kbd "C-k") 'company-select-previous))

;; Flycheck checks your code and helps show alerts from the linter
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (progn
    (setq-default flycheck-phpcs-standard "PSR2")
    (setq-default flycheck-php-phpcs-executable "/bin/phpcs")

    (global-flycheck-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; snippets
  (use-package yasnippet                  ; Snippets
    :ensure t
    :config
    (setq yas-verbosity 1)                      ; No need to be so verbose
    (setq yas-wrap-around-region t)
    (setq yas-snippet-dirs (append yas-snippet-dirs
			       '("~/.emacs.d/snippets")))
    (yas-reload-all)
    (yas-global-mode))

  (use-package yasnippet-snippets         ; Collection of snippets
    :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; yaml-mode
(use-package yaml-mode
  :ensure t
  :mode
  (("\\.yml$" . yaml-mode)
   ("\\.yaml$" . yaml-mode)))

;;; json-mode
(use-package json-mode
  :mode ("\\.json\\'" . json-mode))

;; python
;; pip install "python-language-server[all]" bpython mypy flake8
(use-package python
  :mode (("\\.py\\'" . python-mode))
  :defer t
  :init
  (setq python-indent-offset 4))

(use-package pyenv-mode
  :ensure t
  :init
  (add-to-list 'exec-path "~/.pyenv/shims")
  (setenv "WORKON_HOME" "~/.pyenv/versions/")
  :config
  (pyenv-mode)
  :bind
  ("C-x p e" . pyenv-activate-current-project))

(defun pyenv-init()
  (setq global-pyenv (replace-regexp-in-string "\n" "" (shell-command-to-string "pyenv global")))
  (message (concat "Setting pyenv version to " global-pyenv))
  (pyenv-mode-set global-pyenv)
  (defvar pyenv-current-version nil global-pyenv))

(defun pyenv-activate-current-project ()
  "Automatically activates pyenv version if .python-version file exists."
  (interactive)
  (f-traverse-upwards
   (lambda (path)
     (message path)
     (let ((pyenv-version-path (f-expand ".python-version" path)))
       (if (f-exists? pyenv-version-path)
	  (progn
	    (setq pyenv-current-version (s-trim (f-read-text pyenv-version-path 'utf-8)))
	    (pyenv-mode-set pyenv-current-version)
	    (pyvenv-workon pyenv-current-version)
	    (message (concat "Setting virtualenv to " pyenv-current-version))))))))

(add-hook 'after-init-hook 'pyenv-init)
(add-hook 'projectile-after-switch-project-hook 'pyenv-activate-current-project)

(use-package lsp-python-ms
  :defer t
  :hook (python-mode . (lambda ()
			 (require 'lsp-python-ms)
			 (lsp)))
  :config
  ;; for dev build of language server
  (setq lsp-python-ms-executable
	(expand-file-name "~/Projects/python-language-server/output/bin/Release/Microsoft.Python.LanguageServer.LanguageServer")))

;; golang environment
(use-package go-mode
  :commands go-mode
  :mode (("\\.go?\\'" . go-mode))
  :defer t
  :config
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4)
  (setq tab-width 4))

;; lua environment
(use-package lua-mode
  :ensure t
  :config
  (setq lua-indent-level 4))

;; PHP environment
(use-package php-mode
  :config
  (progn
    (setq-default php-mode-coding-style 'psr2)))

;; leetcode
(require 'leetcode)

;; Org Mode
(use-package htmlize
  :ensure t)

(use-package org
  :defines org-publish-project-alist
  :functions org-publish-find-date org-publish-sitemap-default-entry
  :config
  (defun rk/org-publish-sitemap-time-entry (entry style project)
    "My org sitemap entry with time ENTRY STYLE PROJECT rk short for 9r0k."
    (format "%s %s"
	    (format-time-string
	     "[%Y-%m-%d]"
	     (org-publish-find-date entry project))
	    (org-publish-sitemap-default-entry entry style project)))
  ;;; orgmode 下源码高亮
  (setq org-src-fontify-natively t)
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
 '(lsp-log-io t)
 '(lsp-print-io t)
 '(lsp-print-performance nil)
 '(lsp-trace nil t)
 '(package-selected-packages
   (quote
    (monokai-theme dired-subtree highlight-indent-guides request ms-python company-box yasnippet-snippets yaml-mode which-key web-mode use-package treemacs ssh-deploy solarized-theme smartparens smart-mode-line-powerline-theme shrink-path rainbow-delimiters pyvenv python-mode pyenv-mode prettier-js phpcbf php-mode moe-theme lua-mode lsp-ui lsp-python-ms kubernetes-tramp kubernetes-evil json-mode js2-refactor htmlize helm-rg helm-projectile go-mode flycheck exec-path-from-shell evil-surround evil-nerd-commenter evil-leader evil-escape emmet-mode eldoc-eval dracula-theme doom-themes diminish company-lsp auto-package-update anzu)))
 '(safe-local-variable-values (quote ((encoding . utf-8)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
