(setq org-todo-keywords
      '((sequence "TODO" "STARTED" "|" "DONE" "CANCELED" "POSTPONED")))
(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "yellow" :weight bold)
	      ("STARTED" :foreground "blue" :weight bold)
	      ("DONE" :foreground "green" :weight bold)
	      ("CANCELED" :foreground "red" :weight bold)
	      ("POSTPONED" :foreground "orange" :weight bold))))
(setq org-html-postamble nil) ; no postample (footer)
; export to HTML5
(setq org-html-doctype "html5")
; markdown export
(eval-after-load "org"
  '(require 'ox-md nil t))

; PlantUML mode
(org-babel-do-load-languages
  'org-babel-load-languages
   '(;; other Babel languages
        (plantuml . t)))
(setq org-confirm-babel-evaluate nil)
(setq org-plantuml-jar-path
      (expand-file-name "~/local/opt/plantuml.jar"))

(defun org-doc-header (arg)
  (interactive "P")
  (insert
"#+TITLE: my title
#+AUTHOR: my name
#+DATE: my date
#+OPTIONS: toc:nil
#+OPTIONS: ^:nil
#+LATEX_CLASS: article
#+LATEX_CLASS_OPTIONS: [a4paper,twoside,twocolumn]
#+LATEX_HEADER: \\renewcommand*\\contentsname{Aurkibidea}
#+LATEX_HEADER: \\usepackage{fullpage}
#+LATEX_HEADER: \\usepackage[scaled]{helvet}
#+LATEX_HEADER: \\renewcommand*\\familydefault{\\sfdefault} 
#+LATEX_HEADER: \\usepackage[T1]{fontenc}
#+LATEX_HEADER: \\usepackage{charter}
#+LATEX_HEADER: \\usepackage{sourcecodepro}
#+LATEX_HEADER: \\usepackage[lft]{FiraSans}
#+LATEX_HEADER: \\linespread{1.33}
#+LATEX_HEADER: \\hypersetup{colorlinks=true}
#+LATEX_HEADER: \\hypersetup{linkcolor=black}
#+LATEX_HEADER: \\hypersetup{citecolor=black}
#+LATEX_HEADER: \\hypersetup{filecolor=black}
#+LATEX_HEADER: \\hypersetup{urlcolor=black}
#+HTML_HEAD: <style>body {max-width:33em;margin:0 auto;line-height:1.5em;background:#fff;color:#333;} p{margin:1.5em 0;text-align:justify;} h1{line-height: 1em;} h2{line-height:1.2em} img{max-width:100%;height:auto}</style>

\\tableofcontents
\\newpage
"))
