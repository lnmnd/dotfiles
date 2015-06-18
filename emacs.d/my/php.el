(require 'comint)

;; Enable php-mode
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
;; Use web-mode with templates
(setq php-template-compatibility nil)
;; Enable Symfony coding style
(add-hook 'php-mode-hook 'php-enable-symfony2-coding-style)
;; Enable flycheck
(add-hook 'php-mode-hook 'flycheck-mode)

;; Use psysh as REPL

(setq php-buffer nil)

(defun run-php ()
  (interactive)
  (setq php-buffer (make-comint "PHP" "psysh"))
  (pop-to-buffer php-buffer))

(defun php-eval-region (start end)
  "Send the region between `start' and `end' to PHP REPL for execution.
The output will appear in the buffer *PHP*."
  (interactive "r")
  (comint-send-region php-buffer start end)
  (comint-send-string php-buffer "\n"))
