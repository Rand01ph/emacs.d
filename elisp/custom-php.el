;;; custom-php.el --- -*- lexical-binding: t; -*-

;;; Commentary:
;;; GNU Emacs configuration file

;;; Code:

(use-package php-mode
  :ensure t
  :mode (("\\.php" . php-mode)
	 ("\\.phtml" . php-mode))
  :commands php-mode
  )

(provide 'custom-php)
;;; custom-php.el ends here
