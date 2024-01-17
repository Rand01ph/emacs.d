;;; vim-like.el --- Vim emulation                    -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Core Packages
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Evil: vi emulation
(use-package evil
  :init
  (setq evil-undo-system 'undo-redo)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)       ; prep to load evil-collection
  :config
  (evil-mode 1)
  (evil-set-leader '(motion visual normal) (kbd "SPC"))
  (evil-define-key '(motion visual normal) 'global
    (kbd "<leader>bb") 'switch-to-buffer
    (kbd "<leader>bo") 'switch-to-buffer-other-window
    (kbd "<leader><tab>") 'evil-switch-to-windows-last-buffer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Evil Enhancements
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Evil-collection: handy evil bindings for lots of different modes
(use-package evil-collection
  :after evil
  :config
  (setq evil-want-integration t)
  (evil-collection-init)
  ;; You can add evil-mode keybindings here; for example, when we're in
  ;; special-mode, it usually means we're in some kind of uneditable buffer like
  ;; a message popup etc. `q' will quit inside these windows instead of starting
  ;; a macro recording.
  (evil-collection-define-key 'normal 'special-mode-map
    (kbd "q") 'quit-window))

;; Evil-owl: preview what's in your vim registers
(use-package evil-owl
  :config
  (setq evil-owl-idle-delay 0.3)
  (setq evil-owl-max-string-length 500)
  (add-to-list 'display-buffer-alist
               '("*evil-owl*"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 30)))
  (evil-owl-mode))


;; Evil-escape: Customizable key sequence to escape from insert state and everything else in Emacs
(use-package evil-escape
  :after evil
  :config
  (setq evil-escape-key-sequence "kj")
  (setq evil-escape-delay 0.2)
  (setq evil-escape-unordered-key-sequence t)
  (evil-escape-mode))

;; Enable Commentary
(use-package evil-commentary
  :after evil
  :bind (:map evil-normal-state-map
              ("gc" . evil-commentary)))
