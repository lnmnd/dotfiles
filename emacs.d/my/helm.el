;; helm

(use-package
  helm
  :ensure t
  :config
  (use-package helm-config)
  (helm-mode 1)
  
  (setq helm-buffers-fuzzy-matching t
	helm-recentf-fuzzy-match    t)
  
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x b") 'helm-mini)

  ;; From within a helm-find-files search a file/directory with C-s
  (when (executable-find "ack-grep")
    (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
	  helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f")))


;; helm-projectile
(use-package
  helm-projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))

