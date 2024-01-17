;;; org-init.el --- Org mode config                    -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package org)

(use-package org-contrib
  :after org
  (require 'ox-taskjuggler)
  (setq org-taskjuggler-target-version 3.7.2))
