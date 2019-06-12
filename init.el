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

(setq gc-cons-threshold (* 20 1024 1024))
(setq large-file-warning-threshold 100000000)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;;; config package manager
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

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config))

(use-package smart-mode-line-powerline-theme
  :ensure t)

(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/theme 'powerline)
  (add-hook 'after-init-hook 'sml/setup))

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

(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
(add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode +1))

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
  :ensure t
  :defer 2
  :bind
  ("M-x" . helm-M-x)
  ("C-x C-f" . helm-find-files)
  ("M-y" . helm-show-kill-ring)
  ("C-x r b" . helm-bookmarks)
  ("C-h a" . helm-apropos)
  ("C-h d" . helm-info-at-point)
  ("C-x b" . helm-mini)
  :config
  (require 'helm-config)
  (helm-mode 1)
  (setq helm-split-window-inside-p t
    helm-move-to-line-cycle-in-source t)
  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 20)
  (setq helm-buffers-fuzzy-matching t
	helm-recentf-fuzzy-match t
	helm-M-x-fuzzy-match t)
  (helm-autoresize-mode 1)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
  )

(use-package helm-rg
  :ensure t
  :after helm
  :bind (("C-c k" . helm-rg)))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

(use-package treemacs
  :bind
  (("C-c t" . treemacs)
   ("s-a" . treemacs)))

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


(use-package hydra)

(use-package ssh-deploy
    :ensure t
    :demand
    :after hydra
    :hook ((after-save . ssh-deploy-after-save)
	    (find-file . ssh-deploy-find-file))
    :config
    (ssh-deploy-line-mode) ;; If you want mode-line feature
    (ssh-deploy-add-menu) ;; If you want menu-bar feature
    (ssh-deploy-hydra "C-c C-z") ;; If you want the hydra feature
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;补全和语法检查
;; This is the main mode for LSP
(use-package lsp-mode
  :init (setq lsp-prefer-flymake nil)
  :ensure t)

;; This makes imenu-lsp-minor-mode available. This minor mode
;; will show a table of contents of methods, classes, variables.
;; You can configure it to be on the left by using `configure`

(add-hook 'lsp-after-open-hook 'lsp-enable-imenu)

;; lsp-ui enables the fancy showing of documentation, error
;; messages and type hints
(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-sideline-ignore-duplicate t)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; company is the best autocompletion system for emacs (probably)
;; and this uses the language server to provide semantic completions

(use-package company-lsp
  :commands company-lsp
  :config
  (push 'company-lsp company-backends))
;; I use pyenv to handle my virtual environments, so when I enable
;; pyenv in a Python buffer, it will trigger lsp. Otherwise, it
;; will use the old systems (I think based on jedi)

(add-hook 'pyenv-mode-hook 'lsp)

;; Flycheck checks your code and helps show alerts from the linter
(use-package flycheck
  :init (global-flycheck-mode))

;; Show flake8 errors in lsp-ui
(defun lsp-set-cfg ()
  (let ((lsp-cfg `(:pyls (:configurationSources ("flake8")))))
    (lsp--set-configuration lsp-cfg)))

;; Activate that after lsp has started
(add-hook 'lsp-after-initialize-hook 'lsp-set-cfg)

;; I like proper fonts for documentation, in this case I use the
;; Inter font. High legibility, small size

(add-hook 'lsp-ui-doc-frame-hook
	  (lambda (frame _w)
	    (set-face-attribute 'default frame
		 :font "Noto Sans CJK SC"
		 :height 140)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; yaml-mode
(use-package yaml-mode
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
  (setq python-indent 4)
  :config
  ;; PEP 8 compliant filling rules, 79 chars maximum
  (add-hook 'python-mode-hook #'subword-mode)
  )

(defun pyenv-from-file ()
  (let ((current-file (buffer-file-name))
	(file-name ".python-version"))
    (when current-file
      (let* ((conf-dir (locate-dominating-file current-file file-name))
	     (conf-file (concat conf-dir file-name)))
	(pyenv-mode-set
	 (string-trim (f-read conf-file)))))))

(use-package pyvenv
  :ensure t
  :after python
  :config
  (setenv "WORKON_HOME" (expand-file-name "~/.pyenv/versions")
  (add-hook 'python-mode-hook 'pyenv-from-file))
  )

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
    ("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" default)))
 '(package-selected-packages
   (quote
    (ssh-deploy treemacs rich-minority smart-mode-line-powerline-theme smart-mode-line htmlize doom-modeline doom-themes dracula-theme solarized-theme kubernetes-evil python-mode yasnippet-snippets yaml-mode which-key web-mode use-package smartparens rainbow-delimiters pyenv-mode prettier-js php-mode moe-theme lua-mode lsp-ui lsp-rust lsp-javascript-typescript lsp-html lsp-css kubernetes-tramp kubernetes json-mode js2-refactor helm-rg helm-projectile go-mode flycheck exec-path-from-shell evil-surround evil-nerd-commenter evil-leader evil-escape emmet-mode diminish company-lsp auto-package-update anzu)))
 '(safe-local-variable-values
   (quote
    ((ssh-deploy-on-explicit-save . t)
     (ssh-deploy-root-remote . "/ssh:root@ampil-dev:/opt/disk2/var/www/ampil")
     (ssh-deploy-root-local . "/home/tan/Nsfocus/Projects/Ampil/trunk/api")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
