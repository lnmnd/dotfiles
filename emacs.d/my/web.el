;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq inferior-js-program-command "phantomjs")

;; tern
(add-to-list 'load-path "~/.emacs.d/lib/tern")
(autoload 'tern-mode "~/.emacs.d/lib/tern/emacs/tern.el" nil t)
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))

;; web-mode
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode))
(setq web-mode-ac-sources-alist
      '(("css" . (ac-source-words-in-buffer ac-source-css-property))
        ("html" . (ac-source-words-in-buffer ac-source-abbrev))))

;; mustatche
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . mustache-mode))

;; PHP
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
;; Use web-mode with templates
(setq php-template-compatibility nil)
;; Enable Symfony coding style
(add-hook 'php-mode-hook 'php-enable-symfony2-coding-style)
;; Enable flycheck
(add-hook 'php-mode-hook 'flycheck-mode)

