;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  highlight
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-method 'character)
  :hook
  (css-base-mode . highlight-indent-guides-mode)
  (python-mode . highlight-indent-guides-mode)
  (yaml-ts-mode . highlight-indent-guides-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Motion aids
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package avy
  :bind (("C-c j" . avy-goto-line)
         ("s-j"   . avy-goto-char-timer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Minibuffer and completion
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Vertico: better vertical completion for minibuffer commands
;; Vertico 基于默认完成提供一个高性能且简约的垂直完成 UI 系统。Vertico 经过复用内置设施系统，Vertico 实现了与内置 Emacs 补全的完全兼容命令和完成表。Vertico 仅提供完成 UI，但旨在高度灵活，可扩展和模块化。
;; 如果要插入不存在的对象，例如新建一个 file 或 buffer, 可以使用 M-RET 快捷键（vertico-exit-input)；
(use-package vertico
  :ensure t
  :bind
  (:map vertico-map
	  ;; 关闭 minibuffer。
	("<escape>" . #'abort-minibuffers)
	("TAB" . #'my-vertico-insert-unless-tramp))
  :init
  (fido-mode -1)
  ;; 显示的侯选者数量。
  (setq vertico-count 20)
  (setq vertico-cycle nil)
  (vertico-mode))

;; https://github.com/minad/vertico/issues/240
(defun my-vertico-insert-unless-tramp ()
  "Insert current candidate in minibuffer, except for tramp."
  (interactive)
  (if (vertico--remote-p (vertico--candidate))
      (minibuffer-complete)
    (vertico-insert)))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
	      ("<backspace>" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; 在输入时清理文件路径。
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Marginalia: annotations for minibuffer
(use-package marginalia
  :init
  ;; 显示绝对时间。
  (setq marginalia-max-relative-age 0)
  (marginalia-mode)
  :config
  ;; 文件不添加大小，修改时间等注释，防止 tramp 时卡住。
  (setq marginalia-annotator-registry (assq-delete-all 'file marginalia-annotator-registry))
  (setq marginalia-annotator-registry (assq-delete-all 'project-file marginalia-annotator-registry)))

;; Removing a package
;; To remove a package we need to carry out 3 steps:

;; Remove the package directory from lib
;; Remove its reference in .gitmodules
;; Remove the package directory from .git/modules/lib/
;; Setup a fresh install
;; Just run: git submodule update --init --recursive

;; Popup completion-at-point
(use-package corfu
  :ensure t
  ;; Optional customizations
  :init
  (global-corfu-mode)
  :bind
  (:map corfu-map
	("SPC" . corfu-insert-separator)
	("C-n" . corfu-next)
	("C-p" . corfu-previous))
  :custom
  (corfu-auto t)
  (corfu-quit-no-match 'separator))

;; Part of corfu
(use-package corfu-popupinfo
  :ensure nil
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))



;; Make corfu popup come up in terminal overlay
(use-package corfu-terminal
  :if (not (display-graphic-p))
  :ensure t
  :config
  (corfu-terminal-mode))

;; Pretty icons for corfu
(use-package kind-icon
  :if (display-graphic-p)
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Consult: Misc. enhanced commands
(use-package consult
  :ensure t
  ;; Other good things to bind: consult-ripgrep, consult-line-multi,
  ;; consult-history, consult-outline, consult-error
  :bind (("C-x b" . consult-buffer) ;; orig. switch-to-buffer
         ("M-y" . consult-yank-pop) ;; orig. yank-pop
         ("C-s" . consult-line))    ;; orig. isearch
  :config
  ;; Narrowing lets you restrict results to certain groups of candidates
  (setq consult-narrow-key "<"))


(use-package consult-tramp
  :after consult
  :vc (:url "https://github.com/Ladicle/consult-tramp"
       :branch "main")
  :custom
  ;; 默认为 scpx 模式，不支持 SSH 多跳 Jump。
  (consult-tramp-method "ssh")
  ;; 打开远程的 /root 目录，而非 ~, 避免 tramp hang。
  ;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2007-07/msg00006.html
  (consult-tramp-path "/root/")
  ;; 即使 ~/.ssh/config 正确 Include 了 hosts 文件，这里还是需要配置，因为 consult-tramp 不会解析 Include 配置。
  (consult-tramp-ssh-config "~/work/proxylist/hosts_config"))


(use-package eshell
  :bind (("C-r" . consult-history)))

;; Orderless: powerful completion style
;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))


(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Nyan Cat
(use-package nyan-mode
  :ensure t
  :defer 20
  :config
  (nyan-mode +1))
