;;; setup-python.el --- Setup Python -*- lexical-binding: t -*-

(require 'cider)
(require 'f)
(require 's)

(defun my-eval-python (string point)
  (elpy-shell-get-or-create-process)
  (cider--display-interactive-eval-result
   (python-shell-send-string-no-output string)
   point))

(defun my-eval-python-statement ()
  (interactive)
  (save-excursion
    (let ((start (python-nav-beginning-of-statement))
	  (end (python-nav-end-of-statement)))
      (my-eval-python (buffer-substring start end) end))))

(defun my-eval-python-region ()
  (interactive)
  (let ((region (elpy-shell--region-without-indentation
		 (region-beginning) (region-end))))
    (my-eval-python region (region-end))))

(defun pdb-set-trace ()
  (interactive)
  (move-beginning-of-line 1)
  (insert "import pdb; pdb.set_trace();\n")
  (forward-line -1)
  (indent-line-to python-indent)
  (forward-line)
  (isend-associate "*gud-pdb*"))

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
    (elpy-shell-send-region-or-buffer)))

(defun reset-python ()
  (interactive)
  (switch-to-buffer "*Python*")
  (delete-process (get-buffer-process (current-buffer)))
  (kill-buffer)
  (start-python))

(setq pyenv-activated nil)

(defun find-python-version-dir ()
    (f-traverse-upwards
     (lambda (path)
       (f-exists? (f-expand ".python-version" path)))))

(defun activate-pyenv ()
  (when (not pyenv-activated)
    (let ((python-version-dir (find-python-version-dir)))
      (when python-version-dir
	(let* ((python-version-path (f-expand ".python-version" python-version-dir))
	       (version (s-trim (f-read-text python-version-path 'utf-8))))
	  (pyvenv-activate (concat "~/.pyenv/versions/" version))
	  (setq pyenv-activated t))))))

(use-package
 elpy
 :config
 (elpy-enable)
 (elpy-use-ipython)
 (add-hook 'find-file-hook 'activate-pyenv)
 (setenv "WORKON_HOME" "~/.pyenv/versions")
 (setq elpy-rpc-backend "jedi")
 (setq python-shell-interpreter-args "-i --simple-prompt --pprint")
 (setq elpy-test-django-runner-command '("./manage.py" "test" "--noinput"))
 (setq gud-pdb-command-name "python -m pdb")
 (define-key elpy-mode-map (kbd "C-c C-r") 'my-eval-python-region)
 (define-key elpy-mode-map (kbd "C-c C-e") 'my-eval-python-statement)
 (define-key elpy-mode-map (kbd "<C-return>") 'isend-send))

(provide 'setup-python)
