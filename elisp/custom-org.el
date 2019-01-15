;;; custom-org.el --- Summary
;;; Commentary:

;;; Code:

(use-package htmlize
  :ensure t)

(use-package org
  :ensure nil
  :hook ((org-mode . org-indent-mode)
         (org-indent-mode . (lambda() (diminish 'org-indent-mode))))
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
	   :auto-preamble t
	   :with-toc t

	   :sitemap-date-format "%Y-%m-%d"
	   :sitemap-file-entry-format "%d *%t*"
	   :sitemap-sort-files anti-chronologically
	   :sitemap-filename "index.org"  ; ... call it sitemap.org (it's the default)...
	   :sitemap-title "Just for fun"         ; ... with title 'Sitemap'.
	   :auto-sitemap t                ; Generate sitemap.org automagically...

	   :html-doctype "html5"
	   :html-validation-link nil
	   :html-head "
<link rel=\"shortcut icon\" type=\"image/x-icon\" href=\"/favicon.ico\"/>
<link rel=\"stylesheet\" type=\"text/css\" href=\"/css/org.css\"/> 
<meta name=\"viewport\" content=\"width=device-width; initial-scale=1.0; maximum-scale=1.0; user-scalable=0;\">
<!-- Global site tag (gtag.js) - Google Analytics -->
<script async src=\"https://www.googletagmanager.com/gtag/js?id=UA-67269379-3\"></script>
<script>
  window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag('js', new Date());

  gtag('config', 'UA-67269379-3');
</script>"

	   :html-link-home "index.html"
	   :html-link-up "index.html"

	   :author "Rand01ph"
	   :email "tanyawei1991@gmail.com"
	   :language "zh-CN"
	   :html-head-extra
	   "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://gongzhitaao.org/orgcss/org.css\"/>")

	  ("blog-static"
	   :base-directory "~/Projects/OrgNote/Blog/static"
	   :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf"
	   :publishing-directory "~/Projects/Rand01ph.github.io/static/"
	   :recursive t
	   :publishing-function (org-publish-attachment)))))


;; More fancy UI
(use-package org-bullets
  :ensure t
  :defer
  :hook (org-mode . org-bullets-mode))

(defvar load-language-list '((emacs-lisp . t)
			     (perl . t)
			     (python . t)
			     (ruby . t)
			     (plantuml . t)))


(org-babel-do-load-languages 'org-babel-load-languages
			     load-language-list)


(provide 'custom-org)

;;; custom-org.el ends here
