(deftheme bare "Bare theme")

(setq bare-background "#FBF8EF")
(setq bare-foreground "#000000")
(setq bare-primary "#A5D6A7") ;; green
(setq bare-accent "#FFCC80") ;; orange
(setq bare-red "#E57373")
(setq bare-yellow "#FFF59D")
(setq bare-light-blue "#E1F5FE")

(custom-theme-set-faces
 'bare

 `(comint-highlight-prompt ((t (:foreground ,bare-foreground))))
 `(default ((t (:background ,bare-background :foreground ,bare-foreground))))
 `(font-lock-builtin-face ((t (:background ,bare-background :foreground ,bare-foreground))))
 `(font-lock-comment-delimiter-face ((t (:background ,bare-background :foreground ,bare-foreground))))
 `(font-lock-comment-face ((t (:background ,bare-background :foreground ,bare-foreground))))
 `(font-lock-constant-face ((t (:background ,bare-background :foreground ,bare-foreground))))
 `(font-lock-doc-face ((t (:background ,bare-background :foreground ,bare-foreground))))
 `(font-lock-function-name-face ((t (:background ,bare-background :foreground ,bare-foreground))))
 `(font-lock-keyword-face ((t (:background ,bare-background :foreground ,bare-foreground))))
 `(font-lock-negation-char-face ((t (:background ,bare-background :foreground ,bare-foreground))))
 `(font-lock-preprocessor-face ((t (:background ,bare-background :foreground ,bare-foreground))))
 `(font-lock-regexp-grouping-backslash ((t (:background ,bare-background :foreground ,bare-foreground))))
 `(font-lock-regexp-grouping-construct ((t (:background ,bare-background :foreground ,bare-foreground))))
 `(font-lock-string-face ((t (:background ,bare-background :foreground ,bare-foreground))))
 `(font-lock-type-face ((t (:background ,bare-background :foreground ,bare-foreground))))
 `(font-lock-variable-name-face ((t (:background ,bare-background :foreground ,bare-foreground))))
 `(font-lock-warning-face ((t (:background ,bare-background :foreground ,bare-foreground))))
 `(highlight ((t (:background ,bare-accent))))
 `(isearch ((t (:background ,bare-accent))))
 `(isearch-fail ((t (:background ,bare-red))))
 `(lazy-highlight ((t (:background ,bare-primary))))
 `(mode-line ((t (:background "gray" :foreground "black"))))
 `(mode-line-inactive ((t (:background "#eeeeee" :foreground "black"))))
 `(org-block-begin-line ((t (:background ,bare-primary :foreground ,bare-foreground))))
 `(org-block-end-line ((t (:background ,bare-primary :foreground ,bare-foreground))))
 `(org-level-1 ((t (:height 1.75))))
 `(org-level-2 ((t (:height 1.5))))
 `(org-level-3 ((t (:height 1.25))))
 `(region ((t (:background ,bare-accent))))

 `(avy-lead-face ((t (:background ,bare-accent :foreground ,bare-foreground))))
 `(avy-lead-face-0 ((t (:background ,bare-primary :foreground ,bare-foreground))))
 `(avy-lead-face-1 ((t (:background ,bare-primary :foreground ,bare-foreground))))
 `(avy-lead-face-2 ((t (:background ,bare-primary :foreground ,bare-foreground))))

 `(cider-result-overlay-face ((t (:background ,bare-primary :foreground ,bare-foreground))))

 `(company-scrollbar-bg ((t (:background ,bare-background))))
 `(company-scrollbar-fg ((t (:background ,bare-primary))))
 `(company-tooltip ((t (:background ,bare-background))))
 `(company-tooltip-common ((t (:background ,bare-primary))))
 `(company-tooltip-common-selection ((t (:background ,bare-accent))))
 `(company-tooltip-selection ((t (:background ,bare-accent))))

 `(flycheck-error ((t (:background ,bare-red :underline nil))))
 `(flycheck-warning ((t (:background ,bare-yellow :underline nil))))

 `(ivy-current-match ((t (:background ,bare-accent :foreground ,bare-foreground))))
 `(ivy-minibuffer-match-face-1 ((t (:background ,bare-background :foreground ,bare-foreground))))
 `(ivy-minibuffer-match-face-2 ((t (:background ,bare-primary :foreground ,bare-foreground))))
 `(ivy-minibuffer-match-face-3 ((t (:background ,bare-yellow :foreground ,bare-foreground))))
 `(ivy-minibuffer-match-face-4 ((t (:background ,bare-red :foreground ,bare-foreground))))
 `(ivy-minibuffer-match-highlight ((t (:background ,bare-accent :foreground ,bare-foreground))))

 `(js2-error ((t (:background ,bare-red))))
 `(js2-external-variable ((t (:background ,bare-yellow))))
 `(js2-function-param ((t (:background ,bare-background :foreground ,bare-foreground))))

 `(magit-branch-current ((t (:background ,bare-accent))))
 `(magit-branch-local ((t (:background ,bare-primary))))
 `(magit-branch-remote ((t (:background ,bare-yellow))))
 `(magit-log-author ((t (:background ,bare-background :foreground ,bare-foreground))))
 `(magit-log-date ((t (:background ,bare-background :foreground ,bare-foreground))))
 `(magit-section-highlight ((t (:background ,bare-light-blue))))

 `(popup-tip-face ((t (:background ,bare-primary :foreground ,bare-foreground)))))

(setq hl-paren-colors '())
(setq hl-paren-background-colors (list bare-accent bare-primary bare-yellow bare-red))

(eval-after-load "org"
  '(setq org-todo-keyword-faces
	`(("TODO" . (:background ,bare-red :foreground ,bare-foreground :weight bold))
	  ("STARTED" . (:background ,bare-accent :foreground ,bare-foreground :weight bold))
	  ("DONE" . (:background ,bare-primary :foreground ,bare-foreground :weight bold))
	  ("CANCELED" . (:background ,bare-background :foreground ,bare-foreground :weight bold)))))

(provide-theme 'bare)
