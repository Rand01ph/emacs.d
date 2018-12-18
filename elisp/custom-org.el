;;custom-org.el

(use-package htmlize
  :ensure t)


(use-package org
  :config
  (setq org-publish-project-alist
	'(("blog"
	   :components ("blog-content" "blog-static"))
	  ("blog-content"
	   :base-directory "~/Projects/Rand01ph.github.io/org"
	   :base-extension "org" ;扩展名
	   :publishing-directory "~/Projects/Rand01ph.github.io"
	   :recursive t
	   :publishing-function (org-html-publish-to-html)
	   :headline-levels 4
           :section-numbers nil
	   :auto-preamble t
	   :with-toc t
	   
	   :sitemap-file-entry-format "%d - %t"
	   :sitemap-date-format "%d.%m.%Y"
	   :sitemap-sort-files anti-chronologically
	   :sitemap-filename "index.org"  ; ... call it sitemap.org (it's the default)...
	   :sitemap-title "Just for fun"         ; ... with title 'Sitemap'.
	   :auto-sitemap t                ; Generate sitemap.org automagically...

	   :html-doctype "html5"
	   :html-validation-link nil
	   :html-link-home "index.html"
	   :html-link-up "index.html"

	   :author "Rand01ph"
	   :email "tanyawei1991 at gmail dot com"
           :language "zh-CN"
	   :html-head-extra
	   "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://gongzhitaao.org/orgcss/org.css\"/>")
	  ("blog-static"
	   :base-directory "~/Projects/Rand01ph.github.io/org/static"
	   :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf"
	   :publishing-directory "~/Projects/Rand01ph.github.io/static/"
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
