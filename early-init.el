;;; early-init.el --- earliest birds               -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Startup speed, annoyance suppression
(setq gc-cons-threshold 10000000)
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))

;; Silence stupid startup message
(setq inhibit-startup-echo-area-message (user-login-name))


;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early
(setq tool-bar-mode nil)
(setq menu-bar-mode nil)
(set-scroll-bar-mode nil)
(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)) ; nil for dark text
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

;; Configure keys specific to macOS
(when (featurep 'ns)
  (setq ns-command-modifier 'meta)
  (setq ns-alternate-modifier 'super))

