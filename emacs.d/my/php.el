(require 'comint)

;; Enable php-mode
(use-package
  php-mode
  :ensure t
  :bind
  ("C-c C-z" . run-php)
  ("C-c C-r" . php-eval-region)
  ("C-c C-k" . php-eval-buffer)
  ("C-c C-d" . php-display-doc)
  ("C-c C-s" . php-show-source)
  :init
  ;; Use psysh as REPL
  (setq inferior-php-command "psysh")
  (setq php-buffer nil)

  (defun run-php ()
    (interactive)
    (let ((cmds (split-string inferior-php-command)))
      (setq php-buffer (apply 'make-comint "PHP" (car cmds)
			      nil (cdr cmds))))
    (pop-to-buffer php-buffer))

  (defun php-eval-region (start end)
    "Send the region between `start' and `end' to PHP REPL for execution.
The output will appear in the buffer *PHP*."
    (interactive "r")
    (comint-send-region php-buffer start end)
    (comint-send-string php-buffer "\n"))

  (defun php-send-to-buffer (content)
    (comint-send-string php-buffer content)
    (comint-send-string php-buffer "\n"))

  (defun php-eval-buffer ()
    "Send the buffer to PHP REPL for execution.
The output will appear in the buffer *PHP*."
    (interactive)
    ;; Ignore '<?php' at the beginning of the buffer.
    (let* ((code (buffer-substring (point-min) (point-max)))
	   (cleaned-code (if (string-prefix-p "<?php" code t)
			     (substring code 5)
			   code)))
      (php-send-to-buffer cleaned-code)))

  (defun php-display-doc ()
    (interactive)
    (let ((cmd (concat "doc " (thing-at-point 'symbol))))
      (php-send-to-buffer cmd)))

  (defun php-show-source ()
    (interactive)
    (let ((cmd (concat "show " (thing-at-point 'symbol))))
      (php-send-to-buffer cmd)))
  :config
  (add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
  ;; Use web-mode with templates
  (setq php-template-compatibility nil)
  ;; Enable Symfony coding style
  (add-hook 'php-mode-hook 'php-enable-symfony2-coding-style))

(use-package
  flycheck
  :ensure t
  :config
  (add-hook 'php-mode-hook 'flycheck-mode)
  (setq flycheck-phpmd-rulesets '("cleancode" "codesize" "controversial" "design" "unusedcode"))
  (setq flycheck-phpcs-standard "PSR2"))
