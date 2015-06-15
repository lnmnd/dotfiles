;; helm
(require 'helm)
(require 'helm-config)

(helm-mode 1)
;; open helm buffer inside current window, not occupy whole other window
(setq helm-split-window-in-side-p t)

(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)

;; From within a helm-find-files search a file/directory with C-s
(when (executable-find "ack-grep")
  (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
        helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))

