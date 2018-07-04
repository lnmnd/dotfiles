;;; setup-python.el --- Setup Python -*- lexical-binding: t -*-
;; Install: autopep8, flake8, isort and mypy
;; Install in virtualenv: ipython

(require 'f)
(require 's)

(use-package popup)

(use-package
  pyvenv)

(setq pyenv-activated nil)
(setq python-format-code-activated t)

(defun find-python-version-dir ()
  (locate-dominating-file (buffer-file-name) ".python-version"))

(defun activate-pyenv ()
  (when (not pyenv-activated)
    (let ((python-version-dir (find-python-version-dir)))
      (when python-version-dir
        (let* ((python-version-path (f-expand ".python-version" python-version-dir))
               (version (s-trim (f-read-text python-version-path 'utf-8))))
          (pyvenv-activate (concat "~/.pyenv/versions/" version))
          (setq pyenv-activated t))))))

(defun python-format-code-activate ()
  (interactive)
  (setq python-format-code-activated t))

(defun python-format-code-deactivate ()
  (interactive)
  (setq python-format-code-activated nil))

(defun python-format-code ()
  (when (and python-format-code-activated
	     (eq major-mode 'python-mode))
    (shell-command-to-string (format "isort %s" buffer-file-name))
    (shell-command-to-string (format "autopep8 --in-place %s" buffer-file-name))
    (revert-buffer :ignore-auto :noconfirm)
    (isend-associate "*gud-pdb*")))

(defun pdb-set-trace ()
  (interactive)
  (move-beginning-of-line 1)
  (insert "import pdb; pdb.set_trace();\n")
  (forward-line -1)
  (indent-line-to python-indent)
  (forward-line)
  (isend-associate "*gud-pdb*"))

(defun python-generate-etags ()
  (interactive)
  (let ((dir (projectile-project-root)))
    (shell-command
     (concat "cd " dir " && git ls-files| grep \.py$ | xargs etags -o " dir "TAGS -"))))

(defun python-switch-to-shell ()
  (interactive)
  (when (not (get-buffer "*Python*"))
    (run-python)
    (when (get-buffer "boot.py")
      (switch-to-buffer "boot.py")
      (python-shell-send-buffer)))
  (python-shell-switch-to-shell))

(defun python-doc ()
  (interactive)
  (let* ((symbol (python-info-current-symbol t))
         (str (concat "help(" symbol ")"))
         (output (python-shell-send-string-no-output str)))
    (popup-tip output :point (point-at-bol))))

(defun python-eval-last-statement ()
  (interactive)
  (save-excursion
    (let* ((start (python-nav-beginning-of-statement))
           (end (python-nav-end-of-statement))
           (input (buffer-substring start end))
           (output (python-shell-send-string-no-output input)))
      (popup-tip output :point (point-at-bol)))))

(defun python-eval-print-last-statement ()
  (interactive)
  (save-excursion
    (let* ((start (python-nav-beginning-of-statement))
           (end (python-nav-end-of-statement))
           (input (concat "import pprint; pprint.pprint(" (buffer-substring start end) ")"))
           (output (python-shell-send-string input)))
      (python-shell-switch-to-shell))))

(defun python-reset ()
  (interactive)
  (python-shell-send-string "%reset -f"))

(flycheck-define-checker python-mypy
  ""
  :command ("mypy"
	    "--ignore-missing-imports"
	    source-original)
  :error-patterns
  ((error line-start (file-name) ":" line ": error:" (message) line-end))
  :modes python-mode)

(add-to-list 'flycheck-checkers 'python-mypy t)
(flycheck-add-next-checker 'python-flake8 'python-mypy t)

(defun setup-python--hook ()
  (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "-i --simple-prompt")
  (setq python-shell-completion-native-disabled-interpreters '("ipython"))
  (setq gud-pdb-command-name "python -m pdb")
  (setq flycheck-flake8-maximum-complexity 10)

  (semantic-mode)
  (flycheck-mode)

  (add-hook 'find-file-hook 'activate-pyenv)
  (add-hook 'after-save-hook #'python-format-code)

  (define-key python-mode-map (kbd "<C-return>") 'isend-send)
  (define-key python-mode-map (kbd "C-c C-b") #'python-shell-send-buffer)
  (define-key python-mode-map (kbd "C-c C-d") #'python-doc)
  (define-key python-mode-map (kbd "C-c C-e") #'python-eval-last-statement)
  (define-key python-mode-map (kbd "C-c C-i") #'helm-etags-select)
  (define-key python-mode-map (kbd "C-c C-o") #'helm-semantic-or-imenu)
  (define-key python-mode-map (kbd "C-c C-p") #'python-eval-print-last-statement)
  (define-key python-mode-map (kbd "C-c C-r") #'python-shell-send-region)
  (define-key python-mode-map (kbd "C-c C-t") #'recompile)
  (define-key python-mode-map (kbd "C-c C-x") #'python-reset)
  (define-key python-mode-map (kbd "C-c C-z") #'python-switch-to-shell)
  (define-key python-mode-map (kbd "C-x C-e") #'python-eval-last-statement))

(add-hook 'python-mode-hook #'setup-python--hook)
(add-hook 'python-mode-hook #'enable-show-trailing-whitespace)

(provide 'setup-python)
