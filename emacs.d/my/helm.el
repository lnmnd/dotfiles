;; helm
(use-package
  helm
  :bind
  ("C-x C-f" . helm-find-files)
  ("M-x" . helm-M-x)
  ("C-x b" . helm-mini)
  :init
  (setq helm-buffers-fuzzy-matching t
	helm-recentf-fuzzy-match    t)

  ;; From within a helm-find-files search a file/directory with C-s
  (when (executable-find "ack-grep")
    (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
	  helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f")))  
  :config
  (helm-mode 1)

(use-package helm-ag)

(use-package
  helm-swoop
  :bind
  ("C-c C-g" . helm-swoop))

(use-package
  projectile
  :config
  (projectile-global-mode))

(use-package
  helm-projectile
  :init
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'helm)
  :config
  (helm-projectile-on))
