;; no welcome screen
(setq inhibit-splash-screen t)

(setq initial-scratch-message "")

(setq frame-title-format "%b - emacs")

(load-theme 'bare t)

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

;; highlight matching brackets when your cursor is on one of the bracket
(show-paren-mode 1)

(setq browse-url-browser-function 'eww-browse-url)

(setq compilation-ask-about-save nil)

(setq dired-listing-switches "-alh")
(setq dired-dwim-target t)

(setq-default fill-column 80)

(setq url-privacy-level 'paranoid)

(global-hi-lock-mode 1)

(defun kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun enable-show-trailing-whitespace ()
  (setq show-trailing-whitespace t))

(setq mode-line-format2 nil)

(defun toggle-mode-line ()
  (interactive)
  (let ((tmp mode-line-format))
    (setq-local mode-line-format mode-line-format2)
    (setq-local mode-line-format2 tmp)))

(setq initial-major-mode #'fundamental-mode)

(autoload 'esup "esup" "" t)

(require 'diminish)

(require 'undo-tree)
(diminish 'undo-tree-mode)
(global-undo-tree-mode)

(defun setup-company ()
  (require 'company)
  (diminish 'company-mode)
  ;; keys
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (company-mode-on))

(require 'ivy)
(diminish 'ivy-mode)
(setq ivy-height 20)
(ivy-mode 1)

(require 'counsel)
(diminish 'counsel-mode)
(counsel-mode 1)

(define-key global-map (kbd "C-c C-g") #'swiper)
(define-key global-map (kbd "C-c p f") #'counsel-git)
(define-key global-map (kbd "C-c p s s") #'counsel-git-grep)

(add-to-list 'load-path "~/.emacs.d/lib/magit/lisp")
(autoload 'magit-status "magit")
(define-key global-map (kbd "C-x g") #'magit-status)

(eval-after-load 'flycheck
  '(progn
    (setq flycheck-idle-change-delay 1)
    (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
    (set-face-attribute 'flycheck-error nil :underline "Red1")
    (set-face-attribute 'flycheck-warning nil :underline "orange red")))

(setq avy-keys (nconc (number-sequence ?a ?z) (number-sequence ?A ?Z)))
(eval-after-load 'avy
  '(progn
    (set-face-attribute 'avy-lead-face nil :foreground "red")
    (set-face-attribute 'avy-lead-face nil :background "#fbf8ef")))
(autoload 'avy-goto-word-or-subword-1 "avy")
(define-key global-map (kbd "C-c a j") #'avy-goto-word-or-subword-1)

;; autorevert
;; Call auto-revert-mode

(setq dumb-jump-selector 'ivy)
(setq dumb-jump-prefer-searcher 'ag)
(setq dumb-jump-confirm-jump-to-modified-file nil)

(autoload 'markdown-mode "markdown-mode")
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-hook 'markdown-mode-hook #'auto-fill-mode)

(autoload 'nov-mode "nov")
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(add-to-list 'load-path "~/.emacs.d/lib/pdf-tools/lisp")
(defun setup-pdf-tools ()
  (require 'pdf-annot)
  (require 'pdf-history)
  (require 'pdf-occur)
  (require 'pdf-outline)
  (require 'pdf-sync)
  (require 'pdf-util)
  (require 'pdf-virtual)
  (pdf-tools-install))
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . setup-pdf-tools))

(autoload 'org-recoll-search "org-recoll" "" t)

(eval-after-load 'org '(require 'setup-org))

;; Set elfeed-feeds in custom.el:
;; (setq elfeed-feeds
;;       '(("http://nullprogram.com/feed/" emacs)))
(autoload 'elfeed "elfeed" "" t)

(autoload 'restclient-mode "restclient")
(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))
(defun setup-restclient-mode-hook ()
  (define-key restclient-mode-map (kbd "C-M-x") #'restclient-http-send-current-stay-in-window)  )
(add-hook 'restclient-mode-hook #'setup-restclient-mode-hook)

(dolist (x '(emacs-lisp-mode-hook
	     lisp-mode-hook
	     scheme-mode-hook
	     clojure-mode-hook))
  (add-hook x (lambda ()
		(require 'highlight-parentheses)
		(diminish 'highlight-parentheses-mode)
		(highlight-parentheses-mode t))))

(dolist (x '(emacs-lisp-mode-hook
	     eval-expression-minibuffer-setup-hook
	     ielm-mode-hook
	     lisp-mode-hook
	     lisp-interaction-mode-hook
	     scheme-mode-hook
	     clojure-mode-hook))
  (add-hook x
	    (lambda ()
	      (require 'paredit)
	      (diminish 'paredit-mode)
	      (enable-paredit-mode)
	      (enable-show-trailing-whitespace))))

(defun my-eval-last-sexp ()
  (interactive)
  (require 'popup)
  (let ((result (pp-to-string (eval (pp-last-sexp) lexical-binding))))
    (popup-tip result :point (point-at-bol))))

(defun setup-emacs-lisp-mode-hook ()
  (setup-company)
  (define-key emacs-lisp-mode-map "\C-c\C-h" #'counsel-semantic-or-imenu)
  (define-key emacs-lisp-mode-map "\C-c\C-i" #'xref-find-definitions)
  (define-key emacs-lisp-mode-map "\C-c\C-k" #'eval-buffer)
  (define-key emacs-lisp-mode-map "\C-c\C-o" #'xref-pop-marker-stack)
  (define-key emacs-lisp-mode-map "\C-c\C-p" #'eval-print-last-sexp)
  (define-key emacs-lisp-mode-map "\C-c\C-r" #'eval-region)
  (define-key emacs-lisp-mode-map "\C-x\C-e" #'my-eval-last-sexp))
(add-hook 'emacs-lisp-mode-hook #'setup-emacs-lisp-mode-hook)

(autoload 'clojure-mode "clojure-mode")
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cls\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.clc\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.edn\\'" . clojure-mode))
(eval-after-load 'clojure-mode '(require 'setup-clojure-mode))

(add-hook 'scheme-mode-hook (lambda () (require 'setup-scheme-mode)))

(eval-after-load 'python '(require 'setup-python))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

(autoload 'js2-mode "js2-mode")
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(eval-after-load 'js2-mode '(require 'setup-js2-mode))

(autoload 'web-mode "web-mode")
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode))
(eval-after-load 'web-mode '(require 'setup-web-mode))

(autoload 'css-mode "css-mode")
(add-to-list 'auto-mode-alist '("\\.css\'" . css-mode))
(eval-after-load 'css-mode '(require 'setup-css-mode))

(add-hook 'c-mode-hook #'flycheck-mode)
