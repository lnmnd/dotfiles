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
(defun run-php ()
  (interactive)
  (pop-to-buffer (make-comint "php" "psysh")))
