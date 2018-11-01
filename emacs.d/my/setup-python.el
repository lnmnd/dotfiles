;;; setup-python.el --- Setup Python -*- lexical-binding: t -*-
;; Install: autopep8, flake8, isort and mypy
;; Install in virtualenv: ipython
;; Install for plots: gnuplot and ImageMagick

(require 'dash)
(require 'f)
(require 's)

(use-package popup)

(use-package
  pyvenv)

(use-package
  shx)

(setq pyenv-activated nil)
(setq python-format-code-activated t)

(defun find-python-version-dir ()
  (locate-dominating-file (buffer-file-name) ".python-version"))

(defun activate-pyenv ()
  (interactive)
  (when (not pyenv-activated)
    (let ((python-version-dir (find-python-version-dir)))
      (when python-version-dir
        (let* ((python-version-path (f-expand ".python-version" python-version-dir))
               (version (s-trim (f-read-text python-version-path 'utf-8))))
          (pyvenv-activate (concat "~/.pyenv/versions/" version))
          (setq pyenv-activated t))))))

(defun deactivate-pyenv ()
  (interactive)
  (setq pyenv-activated nil)
  (pyvenv-deactivate))

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
    (revert-buffer :ignore-auto :noconfirm)))

(defun python-switch-to-shell ()
  (interactive)
  (when (not (get-buffer "*Python*"))
    (run-python)
    (python-shell-send-string "exec('try: import pprintpp; pprintpp.monkeypatch()\\nexcept ImportError: pass')")
    (python-shell-send-string "import pickle")
    (python-shell-send-string "def load(filename):")
    (python-shell-send-string "    with open(filename, 'rb') as f:")
    (python-shell-send-string "        return pickle.load(f)")
    (python-shell-send-string "")
    (find-file (expand-file-name "~/.emacs.d/my/py-scope-capture.txt"))
    (python-shell-send-buffer)
    (kill-buffer)
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

(defun python-doc-print ()
  (interactive)
  (let* ((symbol (python-info-current-symbol t))
         (str (concat "help(" symbol ")"))
         (output (python-shell-send-string str)))
    (python-shell-switch-to-shell)))

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

(defun python--plot-add-output (acc val)
  (list (concat (car acc) (format "%s %s\n" (cadr acc) val)) (+ (cadr acc) 1)))

(defun python--plot (cmd)
  (save-excursion
    (let* ((plotline-name (make-temp-file "tmp" nil ".txt"))
	   (start (python-nav-beginning-of-statement))
           (end (python-nav-end-of-statement))
           (input (concat "list(" (buffer-substring start end) ")")))
      (-as-> input x
	     (python-shell-send-string-no-output x)
	     (s-replace "[" "(" x)
	     (s-replace "]" ")" x)
	     (s-replace "," "" x)
	     (read x)
	     (car (seq-reduce #'python--plot-add-output x '("" 1)))
	     (append-to-file x nil plotline-name))
      (python-shell-switch-to-shell)
      (funcall cmd plotline-name))))

(defun python-plotline ()
  (interactive)
  (python--plot #'shx-cmd-plotline))

(defun python-plotbar ()
  (interactive)
  (python--plot #'shx-cmd-plotbar))

(defun python-reset ()
  (interactive)
  (python-shell-send-string "%reset -f"))

(flycheck-define-checker python-mypy
  ""
  :command ("mypy"
	    "--config-file"
            (eval (let ((config-dir (locate-dominating-file (buffer-file-name) "mypy.ini")))
                    (if config-dir
                        (f-expand "mypy.ini" config-dir)
		      (expand-file-name "~/.mypy.ini"))))
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

  (define-key python-mode-map (kbd "C-c C-b") #'python-shell-send-buffer)
  (define-key python-mode-map (kbd "C-c C-d") #'python-doc)
  (define-key python-mode-map (kbd "C-c C-e") #'python-eval-last-statement)
  (define-key python-mode-map (kbd "C-c C-h") #'helm-semantic-or-imenu)
  (define-key python-mode-map (kbd "C-c C-i") #'dumb-jump-go)
  (define-key python-mode-map (kbd "C-c C-o") #'dumb-jump-back)
  (define-key python-mode-map (kbd "C-c C-p") #'python-eval-print-last-statement)
  (define-key python-mode-map (kbd "C-c C-r") #'python-shell-send-region)
  (define-key python-mode-map (kbd "C-c C-t") #'recompile)
  (define-key python-mode-map (kbd "C-c C-x") #'python-reset)
  (define-key python-mode-map (kbd "C-c C-z") #'python-switch-to-shell)
  (define-key python-mode-map (kbd "C-u C-c C-d") #'python-doc-print)
  (define-key python-mode-map (kbd "C-x C-e") #'python-eval-last-statement))

(add-hook 'python-mode-hook #'setup-python--hook)
(add-hook 'python-mode-hook #'enable-show-trailing-whitespace)

(provide 'setup-python)
