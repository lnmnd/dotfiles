(use-package
 elpy
 :ensure t
 :config
 (elpy-enable)
 (setq elpy-rpc-backend "jedi")
 (define-key elpy-mode-map (kbd "C-c C-r") 'elpy-shell-send-region-or-buffer)
 (define-key elpy-mode-map (kbd "C-c C-e") 'elpy-shell-send-current-statement)
 (define-key elpy-mode-map (kbd "C-x C-e") 'elpy-shell-send-current-statement))


(defun pdb-set-trace ()
  (interactive)
  (move-beginning-of-line 1)
  (insert "import pdb; pdb.set_trace();\n")
  (forward-line -1)
  (indent-line-to python-indent)
  (forward-line))
