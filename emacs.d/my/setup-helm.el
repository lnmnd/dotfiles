;;; setup-helm.el --- Setup Helm -*- lexical-binding: t -*-

;; helm
(use-package
  helm
  :diminish helm-mode
  :bind
  ("C-x C-f" . helm-find-files)
  ("M-x" . helm-M-x)
  ("C-x b" . helm-mini)
  :init
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match    t)

  (helm-mode))

(use-package helm-ag)

(use-package
  helm-swoop
  :bind
  ("C-c C-g" . helm-swoop))

(use-package
  helm-projectile
  :init
  (setq projectile-completion-system 'helm)
  :config
  (helm-projectile-on))

(provide 'setup-helm)
