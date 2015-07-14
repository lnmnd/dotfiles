;; helm
(use-package
  helm
  :ensure t
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
  (use-package helm-config)
  (helm-mode 1)

(use-package
  helm-ag
  :ensure t
  :bind
  ("C-c C-g" . helm-do-ag-this-file))

(use-package
  projectile
  :ensure t
  :config
  (projectile-global-mode))

(use-package
  helm-projectile
  :ensure t
  :init
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'helm)
  :config
  (helm-projectile-on))
