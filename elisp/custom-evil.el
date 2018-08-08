;; -*- lexical-binding: t -*-
;; evil config

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1))


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

(provide 'custom-evil)
