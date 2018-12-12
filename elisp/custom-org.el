;;custom-org.el

(use-package org
  :config
  (setq org-publish-project-alist
	'(("blog"
	   :components ("blog-content" "blog-static"))
	  ("blog-content"
	   :base-directory "~/Projects/blog/org"
	   :recursive t
	   :publishing-function (org-html-publish-to-html)
	   :base-extension "org"
	   :html-extension "html"
	   :html-doctype "html5"
	   :publishing-directory "~/Projects/blog/public_html/"
	   :with-toc nil
	   :html-postamble nil
	   :html-preamble t
	   :auto-preamble t
	   :auto-sitemap t                ; Generate sitemap.org automagically...
	   :sitemap-filename "sitemap.org"  ; ... call it sitemap.org (it's the default)...
	   :sitemap-title "Sitemap"         ; ... with title 'Sitemap'.
	   :sitemap-sort-files anti-chronologically
	   :sitemap-file-entry-format "%d %t"
	   :author "Rand01ph"
	   :email "tanyawei1991 at gmail dot com"
	   :sitemap-file-entry-format "%d %t"
	   :sitemap-date-format "%Y-%m"
	   :section-numbers nil
	   :preserve-breaks t
	   :html-head-extra
	   "<link rel=\"stylesheet\" href=\"./static/worg2.css\">"
	   :htmlized-source t
	   :html-head-include-scripts nil
	   :html-head-include-default-style nil)
	  ("blog-static"
	   :base-directory "~/Projects/blog/org/static"
	   :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf"
	   :publishing-directory "~/Projects/blog/public_html/static/"
	   :recursive t
	   :publishing-function (org-publish-attachment)))))

;;; orgmode 下源码高亮
(setq org-src-fontify-natively t)

(setq org-indent-mode t)

;; More fancy UI
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))


(provide 'custom-org)
