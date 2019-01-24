;;; init.el --- Rand01ph's Emacs configurations.  -*- lexical-binding: t; no-byte-compile: nil; -*-

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

;; 括号提示
(use-package smartparens-config
  :ensure smartparens
  :config
  (show-smartparens-global-mode t))

(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
(add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)

;; theme
;; (use-package moe-theme
;;   :ensure t
;;   :config
;;   (load-theme 'moe-dark t))

(use-package doom-themes
  :ensure t
  :init
  (load-theme 'doom-one t))

;; (use-package all-the-icons
;;   :ensure t)

;; (use-package doom-modeline
;;       :ensure t
;;       :hook (after-init . doom-modeline-mode))

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

;;; helm
(use-package helm
  :ensure t
  :defines helm-buffers-fuzzy-matching helm-recentf-fuzzy-match helm-M-x-fuzzy-match
  :bind (("M-x" . helm-M-x)
	 ("C-x b" . helm-mini)
	 ("C-x C-b" . helm-buffers-list)
	 ("M-y" . helm-show-kill-ring)
	 ("C-x C-f" . helm-find-files))
  :config
  (setq helm-buffers-fuzzy-matching t
	helm-recentf-fuzzy-match t
	helm-M-x-fuzzy-match t)
  )

(use-package helm-rg
  :ensure t
  :after helm
  :bind (("C-c k" . helm-rg)))

;;; projectile
;; (use-package projectile
;;   :ensure t
;;   :config
;;   (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;;   (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;;   (projectile-mode +1))

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

;; Org Mode
(use-package htmlize
  :ensure t)

(use-package org
  :defines org-publish-project-alist
  :config
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

	   :sitemap-date-format "%Y-%m-%d"
	   :sitemap-file-entry-format "%d *%t*"
	   :sitemap-sort-files anti-chronologically
	   :sitemap-filename "index.org"  ; ... call it sitemap.org (it's the default)...
	   :sitemap-title "Just for fun"         ; ... with title 'Sitemap'.
	   :auto-sitemap t                ; Generate sitemap.org automagically...

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
 '(package-selected-packages
   (quote
    (htmlize doom-modeline doom-themes dracula-theme solarized-theme kubernetes-evil python-mode yasnippet-snippets yaml-mode which-key web-mode use-package smartparens rainbow-delimiters pyenv-mode prettier-js php-mode moe-theme lua-mode lsp-ui lsp-rust lsp-javascript-typescript lsp-html lsp-css kubernetes-tramp kubernetes json-mode js2-refactor helm-rg helm-projectile go-mode flycheck exec-path-from-shell evil-surround evil-nerd-commenter evil-leader evil-escape emmet-mode diminish company-lsp auto-package-update anzu))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
