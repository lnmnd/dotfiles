;;; setup-python.el --- Setup Python -*- lexical-binding: t -*-
;; Install: autopep8, flake8, isort and mypy
;; Install for plots: gnuplot and ImageMagick

(require 'dash)
(require 'dumb-jump)
(require 'f)
(require 'flycheck)
(require 'popup)
(require 'pyvenv)
(require 's)
(require 'yasnippet)

(setq pyenv-activated nil)
(setq python-format-code-activated t)

(defun find-python-version-dir ()
  (locate-dominating-file (buffer-file-name) ".python-version"))

(defun python--read-text (path)
  (f-read-text path 'utf-8))

(defun activate-pyenv ()
  (interactive)
  (when (not pyenv-activated)
    (let ((python-version-dir (find-python-version-dir)))
      (when python-version-dir
	(->> python-version-dir
	     (f-expand ".python-version")
	     python--read-text
	     s-trim
	     (concat "~/.pyvenv/")
	     pyvenv-activate)
	(setq pyenv-activated t)))))

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
    (python-shell-switch-to-shell)
    (font-lock-mode 0)
    (find-file (expand-file-name "~/.emacs.d/my/boot-python.txt"))
    (python-shell-send-buffer)
    (kill-buffer)
    (when (get-buffer "boot.py")
      (switch-to-buffer "boot.py")
      (python-shell-send-buffer)))
  (python-shell-switch-to-shell))

(defun python-doc ()
  (interactive)
  (-as-> (python-info-current-symbol t) x
	 (concat "help(" x ")")
	 (python-shell-send-string-no-output x)
	 (popup-tip x :point (point-at-bol))))

(defun python-doc-print ()
  (interactive)
  (-as-> (python-info-current-symbol t) x
	 (concat "help(" x ")")
	 (python-shell-send-string x))
  (python-shell-switch-to-shell))

(defun python-eval-last-statement ()
  (interactive)
  (save-excursion
    (let ((start (python-nav-beginning-of-statement))
	  (end (python-nav-end-of-statement)))
      (-as-> (buffer-substring start end) x
	     (python-shell-send-string-no-output x)
	     (popup-tip x :point (point-at-bol))))))

(defun python-eval-print-last-statement ()
  (interactive)
  (save-excursion
    (let ((start (python-nav-beginning-of-statement))
	  (end (python-nav-end-of-statement)))
      (-as-> (buffer-substring start end) x
	     (concat "import pprint; pprint.pprint(" x ")")
	     (python-shell-send-string x)))))

(defun python-undef ()
  (interactive)
  (-as-> (python-info-current-symbol t) x
	 (concat x " = None")
	 (python-shell-send-string-no-output x)))

(defun python-timeit ()
  (interactive)
  (save-excursion
    (let ((start (python-nav-beginning-of-statement))
	  (end (python-nav-end-of-statement)))
      (-as-> (buffer-substring start end) x
             (concat "import timeit; timeit.timeit(lambda: " x ")")
	     (python-shell-send-string x))
      (python-shell-switch-to-shell))))

(defun python--plot-format (point)
  (format "%s %s\n" (car point) (cadr point)))

(defun python--save-plot-file (plot-file)
  (let ((start (python-nav-beginning-of-statement))
	(end (python-nav-end-of-statement)))
    (-as-> (buffer-substring start end) x
	   (concat "list("  x ")")
	   (python-shell-send-string-no-output x)
	   (s-replace "[" "(" x)
	   (s-replace "]" ")" x)
	   (s-replace "," "" x)
	   (read x)
	   (mapconcat #'python--plot-format x "")
	   (append-to-file x nil plot-file))))

(defun python--plot (gnuplot-e)
  (save-excursion
    (let ((plot-file (make-temp-file "tmp" nil ".txt"))
	  (img-file (make-temp-file "tmp" nil ".png")))
      (python--save-plot-file plot-file)
      (let ((status (call-process
		     "gnuplot" nil nil nil "-e"
		     (funcall gnuplot-e plot-file img-file))))
	(when (zerop status)
	  (python-shell-switch-to-shell)
	  (insert-image (create-image img-file))
	  (newline)
	  (run-at-time 1 nil (lambda ()
			       (delete-file plot-file)
			       (delete-file img-file))))))))

(defun python--gnuplot-line-e (plot-file img-file)
  (concat
   "set term png transparent truecolor;"
   "set out \"" img-file "\"; "
   "plot" " \""
   plot-file "\" "
   "with lines linewidth 1 linecolor '#000000' notitle"))

(defun python-plotline ()
  (interactive)
  (python--plot #'python--gnuplot-line-e))

(defun python--gnuplot-bar-e (plot-file img-file)
  (concat
   "set term png transparent truecolor;"
   "set out \"" img-file "\"; "
   "set boxwidth 3;"
   "set style data histograms;"
   "set yrange [0:];"
   "set style fill solid 1.0 border -1;"
   "plot" " \""
   plot-file "\" "
   "u 2:xticlabels(1) notitle"))

(defun python-plotbar ()
  (interactive)
  (python--plot #'python--gnuplot-bar-e))

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

(defun setup-python-hook ()
  (setq python-shell-interpreter "python")
  (setq python-shell-interpreter-args "")
  (setq python-shell-completion-native-disabled-interpreters '("python"))
  (setq gud-pdb-command-name "python -m pdb")
  (setq flycheck-flake8-maximum-complexity 10)

  (semantic-mode)
  (setup-company)
  (flycheck-mode)
  (yas-reload-all)
  (yas-minor-mode)
  (enable-show-trailing-whitespace)

  (add-hook 'find-file-hook 'activate-pyenv)
  (add-hook 'after-save-hook #'python-format-code)

  (define-key python-mode-map (kbd "C-c C-d") #'python-doc)
  (define-key python-mode-map (kbd "C-c C-e") #'python-eval-last-statement)
  (define-key python-mode-map (kbd "C-c C-h") #'counsel-semantic-or-imenu)
  (define-key python-mode-map (kbd "C-c C-i") #'dumb-jump-go)
  (define-key python-mode-map (kbd "C-c C-k") #'python-shell-send-buffer)
  (define-key python-mode-map (kbd "C-c C-l") #'python-plotline)
  (define-key python-mode-map (kbd "C-c C-o") #'dumb-jump-back)
  (define-key python-mode-map (kbd "C-c C-p") #'python-eval-print-last-statement)
  (define-key python-mode-map (kbd "C-c C-r") #'python-shell-send-region)
  (define-key python-mode-map (kbd "C-c C-t") #'recompile)
  (define-key python-mode-map (kbd "C-c C-u") #'python-undef)
  (define-key python-mode-map (kbd "C-c C-z") #'python-switch-to-shell)
  (define-key python-mode-map (kbd "C-u C-c C-d") #'python-doc-print)
  (define-key python-mode-map (kbd "C-x C-e") #'python-eval-last-statement))

(add-hook 'python-mode-hook #'setup-python-hook)

(provide 'setup-python)
