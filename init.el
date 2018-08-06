;;; init.el --- Centaur Emacs configurations.	-*- lexical-binding: t no-byte-compile: t; -*-

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))


(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-linux* (eq system-type 'gnu/linux))
(defconst *is-a-windows* (eq system-type 'windows-nt))
(defconst *is-gui* (display-graphic-p))
(defconst *is-console* (not *is-gui*))


(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;;(set-frame-parameter (selected-frame) 'alpha <both>)
(set-frame-parameter (selected-frame) 'alpha '(85 . 85))
(add-to-list 'default-frame-alist '(alpha . (85 . 85)))


;; common
(setq create-lockfiles nil)
(fset 'yes-or-no-p 'y-or-n-p)


;; packages
(setq package-archives
      '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
	("melpa" . "http://elpa.emacs-china.org/melpa/")
	("org"   . "http://elpa.emacs-china.org/org/")))


(package-initialize)

;; use-package
(unless (package-installed-p 'use-package)
  (progn
    (package-refresh-contents)
    (package-install 'use-package)))


(use-package exec-path-from-shell
  :if (memq window-system '(ns mac))
  :ensure t
  :config (exec-path-from-shell-initialize))


(use-package which-key
	:ensure t
	:config
	(which-key-mode))

(eval-when-compile
  (require 'use-package))
;; 如果插件没有安装，总是自动安装插件
(setq use-package-always-ensure t)
(setq use-package-verbose t)

;; evil config
(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
)

(use-package evil-escape
  :after evil ;; 表示在evil载入后才能载入
  :config
  (progn
    (setq-default evil-escape-key-sequence "kj")
    (setq-default evil-escape-delay 0.2)
    (evil-escape-mode t)
    ))


(use-package evil-leader
  :after evil
  :config
  (progn
    (global-evil-leader-mode t)
    (evil-leader/set-leader "<SPC>")
    ))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-nerd-commenter
  :after evil
  :defer t
  :ensure t
  :commands (evilnc-comment-or-uncomment-lines)
  :init
  (define-key evil-normal-state-map (kbd "s-/") 'evilnc-comment-or-uncomment-lines)
  (define-key evil-visual-state-map (kbd "s-/") 'evilnc-comment-or-uncomment-lines)
)


(use-package diminish)

;; theme
(use-package monokai-theme
  :config
  (load-theme 'monokai t))

(use-package smex
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'smex))

(use-package counsel
  :ensure t)

(use-package swiper
  :ensure t
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-find-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    (global-set-key (kbd "C-c r") 'counsel-rg)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    (advice-add 'swiper :after #'recenter)))

;; mode-line
(use-package anzu)
(defun zilongshanren/update-persp-name ()
(when (bound-and-true-p persp-mode)
    ;; There are multiple implementations of
    ;; persp-mode with different APIs
    (progn
	    (or (not (string= persp-nil-name (safe-persp-name (get-frame-persp))))
		"Default")
	    (let ((name (safe-persp-name (get-frame-persp))))
	    (propertize (concat "[" name "] ")
			'face 'font-lock-preprocessor-face
			'help-echo "Current Layout name.")))))


(defun spaceline--unicode-number (str)
"Return a nice unicode representation of a single-digit number STR."
(cond
    ((string= "1" str) "➊")
    ((string= "2" str) "➋")
    ((string= "3" str) "➌")
    ((string= "4" str) "➍")
    ((string= "5" str) "➎")
    ((string= "6" str) "➏")
    ((string= "7" str) "➐")
    ((string= "8" str) "➑")
    ((string= "9" str) "➒")
    ((string= "0" str) "➓")))

(defun window-number-mode-line ()
"The current window number. Requires `window-numbering-mode' to be enabled."
(when (bound-and-true-p window-numbering-mode)
    (let* ((num (window-numbering-get-number))
	    (str (when num (int-to-string num))))
    (spaceline--unicode-number str))))

(defun mode-line-fill (face reserve)
"Return empty space using FACE and leaving RESERVE space on the right."
(unless reserve
    (setq reserve 20))
(when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
(propertize " "
	    'display `((space :align-to
				(- (+ right right-fringe right-margin) ,reserve)))
	    'face face))

(defun buffer-encoding-abbrev ()
"The line ending convention used in the buffer."
(let ((buf-coding (format "%s" buffer-file-coding-system)))
    (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
	(match-string 1 buf-coding)
    buf-coding)))

(setq my-flycheck-mode-line
    '(:eval
	(pcase flycheck-last-status-change
	(`not-checked nil)
	(`no-checker (propertize " -" 'face 'warning))
	(`running (propertize " ✷" 'face 'success))
	(`errored (propertize " !" 'face 'error))
	(`finished
	    (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
		(no-errors (cdr (assq 'error error-counts)))
		(no-warnings (cdr (assq 'warning error-counts)))
		(face (cond (no-errors 'error)
			    (no-warnings 'warning)
			    (t 'success))))
	    (propertize (format "[%s/%s]" (or no-errors 0) (or no-warnings 0))
			'face face)))
	(`interrupted " -")
	(`suspicious '(propertize " ?" 'face 'warning)))))

(setq-default mode-line-format
	    (list
		" %1"
		'(:eval (propertize
			(window-number-mode-line)
			'face
			'font-lock-type-face))
		" "
		'(:eval (zilongshanren/update-persp-name))

		"%1 "
		;; the buffer name; the file name as a tool tip
		'(:eval (propertize "%b " 'face 'font-lock-keyword-face
				    'help-echo (buffer-file-name)))


		" [" ;; insert vs overwrite mode, input-method in a tooltip
		'(:eval (propertize (if overwrite-mode "Ovr" "Ins")
				    'face 'font-lock-preprocessor-face
				    'help-echo (concat "Buffer is in "
						    (if overwrite-mode
							"overwrite"
							"insert") " mode")))

		;; was this buffer modified since the last save?
		'(:eval (when (buffer-modified-p)
			(concat ","  (propertize "Mod"
						'face 'font-lock-warning-face
						'help-echo "Buffer has been modified"))))

		;; is this buffer read-only?
		'(:eval (when buffer-read-only
			(concat ","  (propertize "RO"
						'face 'font-lock-type-face
						'help-echo "Buffer is read-only"))))
		"] "

		;; anzu
		anzu--mode-line-format

		;; relative position, size of file
		"["
		(propertize "%p" 'face 'font-lock-constant-face) ;; % above top
		"/"
		(propertize "%I" 'face 'font-lock-constant-face) ;; size
		"] "

		;; the current major mode for the buffer.
		'(:eval (propertize "%m" 'face 'font-lock-string-face
				    'help-echo buffer-file-coding-system))

		"%1 "
		my-flycheck-mode-line
		"%1 "
		;; evil state
		'(:eval evil-mode-line-tag)

		;; minor modes
		minor-mode-alist
		" "
		;; git info
		`(vc-mode vc-mode)

		" "

		;; global-mode-string goes in mode-line-misc-info
		mode-line-misc-info

		(mode-line-fill 'mode-line 20)

		;; line and column
		"(" ;; '%02' to set to 2 chars at least; prevents flickering
		(propertize "%02l" 'face 'font-lock-type-face) ","
		(propertize "%02c" 'face 'font-lock-type-face)
		") "

		'(:eval (buffer-encoding-abbrev))
		mode-line-end-spaces
		;; add the time, with the date and the emacs uptime in the tooltip
		;; '(:eval (propertize (format-time-string "%H:%M")
		;;                     'help-echo
		;;                     (concat (format-time-string "%c; ")
		;;                             (emacs-uptime "Uptime:%hh"))))
		))


;; font
;; M+ 1m    Noto Sans CJK SC
(defun my-default-font()
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
  ;; 我的字体
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


;; projects
(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (projectile-mode +1))


(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)

  (show-smartparens-global-mode -1)
  (smartparens-global-mode t)

  (setq sp-autoescape-string-quote nil)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-highlight-pair-overlay nil)
  (setq sp-highlight-wrap-overlay nil)
  (setq sp-highlight-wrap-tag-overlay nil)
  (setq sp-show-pair-delay 0)
  (setq sp-show-pair-from-inside t)

  ;; Disable ' pairing where it makes sense.
  (sp-local-pair 'calc-mode "'" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'ielm-mode "'" nil :actions nil)
  (sp-local-pair 'latex-mode "'" nil :actions nil)
  (sp-local-pair 'lisp-interaction-mode "'" nil :actions nil)
  (sp-local-pair 'lisp-mode "'" nil :actions nil)
  (sp-local-pair 'log-edit-mode "'" nil :actions nil)
  (sp-local-pair 'org-mode "'" nil :actions nil)
  (sp-local-pair 'tex-mode "'" nil :actions nil)
  (sp-local-pair 'text-mode "'" nil :actions nil)

  ;; Enable smartparens in minibuffer
  (setq sp-ignore-modes-list
        (delete 'minibuffer-inactive-mode sp-ignore-modes-list))
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

  (defun ef-sp-create-newline-and-enter-sexp (&rest _ignored)
    "Open a new brace or bracket expression, with relevant newlines and indent"
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode)))


(use-package rainbow-delimiters
  :ensure t
  :config
  ;; Only load rainbow-delimiters-mode in prog-mode so it doesn't
  ;; break diffs in magit.
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; company

(use-package company
  :diminish company-mode
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :preface
  (defvar company-enable-yas t
    "Enable yasnippet for all backends.")

  (defun company-backend-with-yas (backend)
    (if (or (not company-enable-yas)
            (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  :bind (("M-/" . company-complete)
         ("C-c C-y" . company-yasnippet)
         :map company-active-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)
         ("TAB" . company-complete-common-or-cycle)
         ("<tab>" . company-complete-common-or-cycle)
         ("S-TAB" . company-select-previous)
         ("<backtab>" . company-select-previous)
         :map company-search-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next))
  :hook (after-init . global-company-mode)
  :config
  (setq company-tooltip-align-annotations t ; aligns annotation to the right
        company-tooltip-limit 12            ; bigger popup window
        company-idle-delay .2               ; decrease delay before autocompletion popup shows
        company-echo-delay 0                ; remove annoying blinking
        company-minimum-prefix-length 2
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil)

  ;; Popup documentation for completion candidates
  (when (display-graphic-p)
    (use-package company-quickhelp
      :bind (:map company-active-map
                  ("M-h" . company-quickhelp-manual-begin))
      :hook (global-company-mode . company-quickhelp-mode)
      :config (setq company-quickhelp-delay 0.8)))

  ;; Support yas in commpany
  ;; Note: Must be the last to involve all backends
  (setq company-backends (mapcar #'company-backend-with-yas company-backends)))


;; Emacs client for the Language Server Protocol
;; https://github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
  :diminish lsp-mode
  :config
  (setq lsp-inhibit-message t)

  ;; https://emacs-china.org/t/topic/6392/2
  (defun restart-lsp-server ()
    "Restart LSP server."
    (interactive)
    (setq lsp--workspaces (make-hash-table :test #'equal))
    (revert-buffer t t)
    (message "LSP server restarted."))

  (require 'lsp-imenu)
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
  (lsp-define-stdio-client lsp-python "python"
			   (lsp-make-traverser #'(lambda (dir)
						   (directory-files
						    dir
						    nil
						    "\\(__init__\\|setup\\)\\.py")))
			   '("pyls"))
  )

(use-package lsp-ui
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
  :hook (lsp-mode . lsp-ui-mode))


(use-package company-lsp
  :after company
  :defines company-backends
  :functions company-backend-with-yas
  :init (cl-pushnew (company-backend-with-yas 'company-lsp) company-backends))


;; Python support for lsp-mode using pyls.
;; Install: pip install python-language-server
(use-package lsp-python
  :commands lsp-python-enable
  :hook (python-mode . lsp-python-enable))


;; Rust support for lsp-mode using the Rust Language Server.
;; Install: rustup component add rls-preview rust-analysis rust-src
(use-package lsp-rust
  :commands lsp-rust-enable
  :hook (rust-mode . lsp-rust-enable))


;; Python
(use-package pyenv-mode
  :init
  (add-to-list 'exec-path "~/.pyenv/shims")
  (setenv "WORKON_HOME" "~/.pyenv/versions/")
  :config
  (pyenv-mode)
  :hook python-mode)


;; flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" default)))
 '(package-selected-packages
   (quote
    (rainbow-delimiters smartparens smex evil-nerd-commenter pyenv-mode lsp-rust lsp-python evil-surround which-key exec-path-from-shell company-quickhelp general evil-collection projectile company-lsp lsp-ui lsp-mode auto-compile diminish anzu monokai-theme evil-leader evil-escape use-package evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
