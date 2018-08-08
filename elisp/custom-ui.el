
;;; Disable toolbar & menubar & scroll-bar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;;(set-frame-parameter (selected-frame) 'alpha <both>)
(set-frame-parameter (selected-frame) 'alpha '(85 . 85))
(add-to-list 'default-frame-alist '(alpha . (85 . 85)))


;; mode-line

;; This package allow us hide or abbreviate the mode line displays of
;; minor-modes.
(use-package diminish
  :ensure t)

(use-package anzu
  :ensure t
  :config
  (global-anzu-mode))

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


;; theme
;; (use-package monokai-theme
;;   :ensure t
;;   :init
;;   (load-theme 'monokai t))

(use-package moe-theme
  :ensure t
  :config
  (load-theme 'moe-dark t))

(provide 'custom-ui)
