(use-package
 elpy
 :ensure t
 :config
 (elpy-enable)
 (elpy-use-ipython)
 (setq elpy-rpc-python-command "python3")
 (setq elpy-rpc-backend "jedi")
 (setq python-shell-interpreter-args "-i --simple-prompt --pprint")
 (define-key elpy-mode-map (kbd "C-c C-r") 'elpy-shell-send-region-or-buffer)
 (define-key elpy-mode-map (kbd "C-c C-e") 'elpy-shell-send-current-statement)
 (define-key elpy-mode-map (kbd "C-x C-e") 'elpy-shell-send-current-statement)
 (define-key elpy-mode-map (kbd "<C-return>") 'isend-send)
 (setq elpy-test-django-runner-command '("./manage.py" "test" "--noinput"))
 (setq gud-pdb-command-name "python -m pdb"))


(defun pdb-set-trace ()
  (interactive)
  (move-beginning-of-line 1)
  (insert "import pdb; pdb.set_trace();\n")
  (forward-line -1)
  (indent-line-to python-indent)
  (forward-line))

(defun start-python ()
  (interactive)
  (elpy-shell-get-or-create-process)
  (find-file (expand-file-name "~/.emacs.d/my/start-python.txt"))
  (mark-whole-buffer)
  (elpy-shell-send-region-or-buffer)
  (kill-buffer)
  (when (get-buffer "boot.py")
    (switch-to-buffer "boot.py")
    (mark-whole-buffer)
    (elpy-shell-send-region-or-buffer))
  (switch-to-buffer "*Python*"))

(defun reset-python ()
  (interactive)
  (switch-to-buffer "*Python*")
  (delete-process (get-buffer-process (current-buffer)))
  (kill-buffer)
  (start-python))
