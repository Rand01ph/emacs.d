;;; custom-const.el ---  -*- lexical-binding: t -*-
;;; Commentary:

;;; code:

(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-linux* (eq system-type 'gnu/linux))
(defconst *is-a-windows* (eq system-type 'windows-nt))
(defconst *is-gui* (display-graphic-p))
(defconst *is-console* (not *is-gui*))

(provide 'custom-const)
;;; custom-const ends here
