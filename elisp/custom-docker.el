;;; custom-docker.el --- kubernetes tools configuration

;;; Commentary:

;;; Code:


;;; [ kubernetes ] -- Emacs porcelain for Kubernetes. A magit-style interface to the Kubernetes command-line client.

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))

;;; [ kubernetes-tramp ] -- offers a TRAMP method for Docker containers deployed in a Kubernetes cluster.

(use-package kubernetes-tramp
  :ensure t)

(provide 'custom-docker)
;;; custom-docker.el ends here
