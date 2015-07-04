(require 'comint)

;; Enable php-mode
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
;; Use web-mode with templates
(setq php-template-compatibility nil)
;; Enable Symfony coding style
(add-hook 'php-mode-hook 'php-enable-symfony2-coding-style)

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

(defvar php-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "\C-c\C-z" 'run-php)
    (define-key m "\C-c\C-r" 'php-eval-region)
    (define-key m "\C-c\C-k" 'php-eval-buffer)
    (define-key m "\C-c\C-d" 'php-display-doc)
    (define-key m "\C-c\C-s" 'php-show-source)
    m))

(use-local-map php-mode-map)

(use-package
  flycheck
  :ensure t
  :config
  (add-hook 'php-mode-hook 'flycheck-mode)
  (setq flycheck-phpmd-rulesets '("cleancode" "codesize" "controversial" "design" "unusedcode"))
  (setq flycheck-phpcs-standard "PSR2"))
