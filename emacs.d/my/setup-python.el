;;; setup-python.el --- Setup Python -*- lexical-binding: t -*-

(use-package
  pyvenv)

(defun pdb-set-trace ()
  (interactive)
  (move-beginning-of-line 1)
  (insert "import pdb; pdb.set_trace();\n")
  (forward-line -1)
  (indent-line-to python-indent)
  (forward-line)
  (isend-associate "*gud-pdb*"))

(defun my-python-generate-etags ()
  (interactive)
  (let ((dir (projectile-project-root)))
    (shell-command
     (concat "cd " dir " && git ls-files| grep \.py$ | xargs etags -o " dir "TAGS -"))))

(defun my-python-switch-to-shell ()
  (interactive)
  (when (not (get-buffer "*Python*"))
    (run-python)
    (find-file (expand-file-name "~/.emacs.d/my/start-python.txt"))
    (python-shell-send-buffer)
    (kill-buffer)
    (when (get-buffer "boot.py")
      (switch-to-buffer "boot.py")
      (python-shell-send-buffer)))
  (python-shell-switch-to-shell))

(defun my-python-doc ()
  (interactive)
  (let* ((symbol (python-info-current-symbol t))
	 (str (concat "help(" symbol ")")))
    (python-shell-send-string str)))

(defun my-python-eval-last-statement ()
  (interactive)
  (save-excursion
    (let ((start (python-nav-beginning-of-statement))
	  (end (python-nav-end-of-statement)))
      (python-shell-send-string (buffer-substring start end)))))

(defun setup-python--hook ()
  (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "-i --simple-prompt")
  (setq python-shell-completion-native-disabled-interpreters '("ipython"))
  (setq gud-pdb-command-name "python -m pdb")

  (semantic-mode)
  (flycheck-mode)

  (add-hook 'after-save-hook #'my-python-generate-etags)

  (define-key python-mode-map (kbd "<C-return>") 'isend-send)
  (define-key python-mode-map (kbd "C-c C-b") #'python-shell-send-buffer)
  (define-key python-mode-map (kbd "C-c C-d") #'my-python-doc)
  (define-key python-mode-map (kbd "C-c C-e") #'my-python-eval-last-statement)
  (define-key python-mode-map (kbd "C-c C-i") #'helm-etags-select)
  (define-key python-mode-map (kbd "C-c C-o") #'helm-semantic-or-imenu)
  (define-key python-mode-map (kbd "C-c C-r") #'python-shell-send-region)
  (define-key python-mode-map (kbd "C-c C-z") #'my-python-switch-to-shell)
  (define-key python-mode-map (kbd "C-x C-e") #'my-python-eval-last-statement))

(add-hook 'python-mode-hook #'setup-python--hook)

(provide 'setup-python)
