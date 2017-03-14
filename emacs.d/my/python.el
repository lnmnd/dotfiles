(defun my-elpy-shell-send-current-statement ()
  "Works like elpy-shell-send-current-statement but doesn't switch to Python shell"
  (interactive)
  (let ((beg (python-nav-beginning-of-statement))
        (end (python-nav-end-of-statement)))
    (elpy-shell-get-or-create-process)
    (print (python-shell-send-string-no-output (buffer-substring beg end)))))

(defun my-elpy-shell-send-region-or-buffer (&optional arg)
  "Works like elpy-shell-send-region-or-buffer but doesn't switch to Python shell"
  (interactive "P")
  ;; Ensure process exists
  (elpy-shell-get-or-create-process)
  (let ((if-main-regex "^if +__name__ +== +[\"']__main__[\"'] *:")
        (has-if-main nil))
    (if (use-region-p)
        (let ((region (elpy-shell--region-without-indentation
                       (region-beginning) (region-end))))
          (setq has-if-main (string-match if-main-regex region))
          (when (string-match "\t" region)
            (message "Region contained tabs, this might cause weird errors"))
          (print (python-shell-send-string-no-output region)))
      (save-excursion
        (goto-char (point-min))
        (setq has-if-main (re-search-forward if-main-regex nil t)))
      (python-shell-send-buffer arg))
    (when has-if-main
      (message (concat "Removed if __main__ == '__main__' construct, "
                       "use a prefix argument to evaluate.")))))


(use-package
 elpy
 :config
 (elpy-enable)
 (elpy-use-ipython)
 (setq elpy-rpc-python-command "python3")
 (setq elpy-rpc-backend "jedi")
 (setq python-shell-interpreter-args "-i --simple-prompt --pprint")
 (define-key elpy-mode-map (kbd "C-c C-r") 'my-elpy-shell-send-region-or-buffer)
 (define-key elpy-mode-map (kbd "C-c C-e") 'my-elpy-shell-send-current-statement)
 (define-key elpy-mode-map (kbd "C-x C-e") 'my-elpy-shell-send-current-statement)
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
    (elpy-shell-send-region-or-buffer)))

(defun reset-python ()
  (interactive)
  (switch-to-buffer "*Python*")
  (delete-process (get-buffer-process (current-buffer)))
  (kill-buffer)
  (start-python))
