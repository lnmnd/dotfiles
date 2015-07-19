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

;; Don't remove the top menu
(menu-bar-mode 1)
;;; Remove the tool bar
(tool-bar-mode -1)

;; clock
(setq display-time-format "%H:%M")
(display-time)

(set-frame-font "Source Code Pro-12")

(custom-set-variables
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (wombat))))

;; Set the return key to globally act as a new-line-and-intent 
(global-set-key "\C-m" 'newline-and-indent)

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
  irony
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode))

(use-package
  company-irony
  :ensure t
  :config
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-irony))
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands))

(use-package
  flycheck
  :ensure t
  :config
  (add-hook 'php-mode-hook 'flycheck-mode)
  (setq flycheck-phpmd-rulesets '("cleancode" "codesize" "controversial" "design" "unusedcode"))
  (setq flycheck-phpcs-standard "PSR2")

  (add-hook 'c-mode-hook 'flycheck-mode))

(use-package
  flycheck-irony
  :ensure t
  :config
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))
