(defun my/byte-recompile ()
  (interactive)
  (byte-recompile-directory "~/.emacs.d" 0))

;; backups
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; no welcome screen
(setq inhibit-splash-screen t)

;; Remove the menu bar
(menu-bar-mode -1)
;;; Remove the tool bar
(tool-bar-mode -1)

;; clock
(setq display-time-format "%H:%M")
(display-time)

(set-frame-font "Source Code Pro-12")
(setq initial-scratch-message ";; scratch\n\n(set-frame-font \"Source Code Pro 12\")")

(custom-set-variables
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (wombat))))

;; Set the return key to globally act as a new-line-and-intent 
(global-set-key "\C-m" 'newline-and-indent)

(define-key key-translation-map (kbd "<f9> l") (kbd "λ"))

;; use aspell, basque, ultra fast mode
(setq ispell-program-name "aspell")
(setq ispell-dictionary "euskera")
(setq ispell-extra-args '("--sug-mode=ultra"))

;; insert date
(defun date (arg)
  (interactive "P")
  (insert (if arg
	      (format-time-string "%d.%m.%Y")
	    (format-time-string "%Y-%m-%d"))))

;; no rebase-mode
(setq auto-mode-alist (delete '("git-rebase-todo" . rebase-mode)
			      auto-mode-alist))

(electric-pair-mode)

(use-package
  undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

;; global company mode
(use-package
  company
  :ensure t
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
  magit
  :ensure t
  :config
  (setq magit-last-seen-setup-instructions "1.4.0"))

(use-package
  haskell-mode
  :ensure t
  :config
  (setq haskell-program-name "stack ghci")
  (define-key haskell-mode-map (kbd "C-x C-d") nil)
  (define-key haskell-mode-map (kbd "C-c M-.") nil)
  (define-key haskell-mode-map (kbd "C-c C-d") nil)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
  (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info))

(use-package
  flycheck
  :ensure t
  :config
  (add-hook 'php-mode-hook 'flycheck-mode)
  (setq flycheck-phpmd-rulesets '("cleancode" "codesize" "controversial" "design" "unusedcode"))
  (setq flycheck-phpcs-standard "PSR2")

  (add-hook 'c-mode-hook 'flycheck-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode))


(use-package
  rust-mode
  :ensure t)

(use-package
  flymake-rust
  :ensure t
  :config
  (add-hook 'rust-mode-hook 'flymake-rust-load))

(use-package
  toml-mode
  :ensure t)

(use-package
  gherkin-mode
  :ensure t)

(use-package
  yaml-mode
  :ensure t)

(use-package
  markdown-mode
  :ensure t)

(use-package
  json-mode
  :ensure t)

(use-package
  csv-mode
  :ensure t)

(use-package
  foreman-mode
  :ensure t)
