(use-package
 elpy
 :ensure t
 :config
 (elpy-enable)
 (setq elpy-rpc-backend "jedi")
 (define-key elpy-mode-map (kbd "C-c C-r") 'elpy-shell-send-region-or-buffer))


