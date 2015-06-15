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
(add-to-list 'auto-mode-alist '("\\.php?\\'" . web-mode))
(setq web-mode-code-indent-offset 4)
(add-hook 'web-mode-hook (lambda () (flymake-mode-on)))
(setq web-mode-ac-sources-alist
      '(("css" . (ac-source-words-in-buffer ac-source-css-property))
        ("html" . (ac-source-words-in-buffer ac-source-abbrev))
        ("php" . (ac-source-words-in-buffer
                  ac-source-words-in-same-mode-buffers
                  ac-source-dictionary))))

;; mustatche
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . mustache-mode))

;; 4 space indent, no tabs
(setq php-mode-force-pear t)
