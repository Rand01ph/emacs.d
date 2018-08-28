;;custom-org.el

;;; orgmode 下源码高亮
(setq org-src-fontify-natively t)

;; More fancy UI
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))


(provide 'custom-org)
