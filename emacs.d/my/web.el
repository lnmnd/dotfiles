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
;; no tabs
(setq-default indent-tabs-mode nil)
;; 2 spaces
(setq web-mode-markup-indent-offset 2)
(setq web-mode-ac-sources-alist
      '(("css" . (ac-source-words-in-buffer ac-source-css-property))
        ("html" . (ac-source-words-in-buffer ac-source-abbrev))))

;; mustatche
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . mustache-mode))
