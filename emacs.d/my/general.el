;;; general.el --- General -*- lexical-binding: t -*-

;; no welcome screen
(setq inhibit-splash-screen t)

;; Remove the menu bar
(menu-bar-mode -1)
;;; Remove the tool bar
(tool-bar-mode -1)
;; Remove the scroll bar
(scroll-bar-mode -1)

(setq frame-title-format "%b - emacs")

(add-to-list 'default-frame-alist '(font . "Source Code Pro-12"))
(custom-set-variables
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (tango))))

;; Mode line
(set-face-attribute 'mode-line nil :box nil :background "#bdbdbd" :foreground "#333333")
(set-face-attribute 'mode-line-inactive nil :box nil :background "#e0e0e0" :foreground "#333333")

;; backups
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(fset 'yes-or-no-p 'y-or-n-p)

;; Set the return key to globally act as a new-line-and-intent
(global-set-key "\C-m" 'newline-and-indent)

(define-key key-translation-map (kbd "<f9> t") (kbd "~"))
(define-key key-translation-map (kbd "<f9> l") (kbd "λ"))

;; no rebase-mode
(setq auto-mode-alist (delete '("git-rebase-todo" . rebase-mode)
                              auto-mode-alist))

(setq vc-follow-symlinks t)
(electric-pair-mode)

(setq tags-revert-without-query 1)

;; highlight matching brackets when your cursor is on one of the bracket
(show-paren-mode 1)

(setq browse-url-browser-function 'eww-browse-url)

(defun kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun enable-show-trailing-whitespace ()
  (setq show-trailing-whitespace t))

(use-package diminish)

(use-package
  undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

;; global company mode
(use-package
  company
  :diminish company-mode
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  ;; auto-completion starts from one character
  (setq company-minimum-prefix-length 1)
  ;; keys
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

(use-package
  projectile
  :diminish projectile-mode
  :init
  (setq projectile-enable-caching t)
  :config
  (projectile-global-mode))

(use-package
  magit
  :config
  (setq magit-last-seen-setup-instructions "1.4.0"))

(use-package
  flycheck
  :config
  (add-hook 'php-mode-hook 'flycheck-mode)
  (setq flycheck-phpmd-rulesets '("cleancode"
                                  "codesize"
                                  "controversial"
                                  "design"
                                  "unusedcode"))
  (setq flycheck-phpcs-standard "PSR2")
  (set-face-attribute 'flycheck-error nil :underline "Red1")
  (set-face-attribute 'flycheck-warning nil :underline "orange red")

  (add-hook 'c-mode-hook 'flycheck-mode))

(use-package
  isend-mode
  :config
  ;; C-RET to send the current line to the interpreter
  (setq isend-forward-line nil)
  (setq isend-skip-empty-lines nil)
  (setq isend-strip-empty-lines nil)
  (setq isend-delete-indentation t)
  (setq isend-end-with-empty-line t))

(use-package
  avy
  :config
  (setq avy-keys (nconc (number-sequence ?a ?z) (number-sequence ?A ?Z)))
  :bind
  ("C-c a j" . avy-goto-word-or-subword-1))

;; Set elfeed-feds in custom.el:
;; (setq elfeed-feeds
;;       '(("http://nullprogram.com/feed/" emacs)
;;         ("http://planet.emacsen.org/atom.xml" emacs)))
(use-package
  elfeed)

(use-package
  markdown-mode
  :config
  (add-hook 'markdown-mode-hook #'enable-show-trailing-whitespace))

(use-package
  json-mode
  :config
  (add-hook 'json-mode-hook #'enable-show-trailing-whitespace))
(use-package
  yaml-mode
  :config
  (add-hook 'yaml-mode-hook #'enable-show-trailing-whitespace))

(use-package
  csv-mode
  :config
  (add-hook 'csv-mode-hook #'enable-show-trailing-whitespace))

(use-package
  lua-mode
  :config
  (add-hook 'lua-mode-hook #'enable-show-trailing-whitespace))

(use-package
  autorevert
  :diminish auto-revert-mode)

(use-package yasnippet
  :diminish yas-minor-mode
  :config (yas-global-mode))

(use-package recompile-on-save)
(use-package suggest)
(use-package writeroom-mode)

(provide 'general)
