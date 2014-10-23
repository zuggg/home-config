(require 'mode-tex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HOOKS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook-and-eval
 'latex-mode-hook
 (lambda ()
   (set (make-local-variable 'tex-extension-list)
        '("aux" "bbl" "blg" "glg" "glo" "gls" "idx" "ilg" "ind" "lof" "log" "maf" "mt" "mtc" "nav" "out" "snm" "synctex" "synctex.gz" "tns" "toc" "xdy"))
   (set (make-local-variable 'tex-command) "pdflatex")
   ;; For some unknown reasons, `skeleton-end-hook' is set to nil in tex-mode.
   (add-hook 'skeleton-end-hook 'skeleton-make-markers)
   (local-set-key (kbd "C-c p") 'latex-math-preview-expression)
   (local-set-key (kbd "C-c j") 'latex-math-preview-insert-symbol)
   (local-set-key (kbd "C-c C-j") 'latex-math-preview-last-symbol-again)
   (local-set-key (kbd "C-c a") 'latex-article)
   (local-set-key (kbd "C-c u") 'latex-package)
   (local-set-key (kbd "C-c C-e") 'latex-environment)
   (local-set-key (kbd "C-c C-d") 'latex-displayed-math)
   (local-set-key (kbd "C-c d") 'latex-inline-math)
   (local-set-key (kbd "C-c C-w") 'latex-macro)
   (local-set-key (kbd "C-c C-s") 'latex-insert-section)
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LATEX-MATH-PREVIEW
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'latex-math-preview-expression "latex-math-preview" nil t)
(autoload 'latex-math-preview-insert-symbol "latex-math-preview" nil t)
(autoload 'latex-math-preview-save-image-file "latex-math-preview" nil t)
(autoload 'latex-math-preview-beamer-frame "latex-math-preview" nil t)

(setq latex-math-preview-cache-directory-for-insertion
      (concat emacs-cache-folder "latex-math-preview-cache"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SKELETONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-skeleton latex-package "Use package." "Package: " \n "\\usepackage[" @ "]{" @ _ "}" \n @)
(define-skeleton latex-macro "Inserts a macro \foo{}, and places the cursor inside the brackets." "Macro:" "\\" str "{"@ _"}" @)

(defvar latex-section-default "section")
(defvar latex-section-names
  '("part" "part*" "chapter" "chapter*" "section*" "subsection" "subsection*"
    "subsubsection" "subsubsection*" "paragraph" "paragraph*" "subparagraph" "subparagraph*")
  "Standard LaTeX section names.")

(define-skeleton latex-insert-section
  "Insert section at point.
Puts point to section title. Section are auto-completed from
`latex-section-names'."
  (let ((choice (completing-read (format "LaTeX section name [%s]: "
                                         latex-section-default)
                                 latex-section-names
                                 nil nil nil nil latex-section-default)))
    (setq latex-section-default choice)
    (unless (member choice latex-section-names)
      ;; Remember new block names for later completion.
      (push choice latex-section-names))
    choice)
  \n "\\" str "{" @ _ "}" @)


(define-skeleton latex-environment
  "Inserts a latex environment into current buffer."
  "Environment: "
  > "\\begin{"str"}"\n
  > @ _ \n
  > "\\end{"str"}")

(define-skeleton latex-displayed-math
  "Inserts displayed math environment."
  nil
  > "\\[\n"
  > @ _ "\n"
  > "\\]"@)

(define-skeleton latex-inline-math
  "Inserts inline math environment."
  nil
  "\\(" @ _ "\\)"@)

(define-skeleton latex-article
  "Inserts article template."
  nil
  "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\\documentclass[a4paper]{article}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}

\\usepackage{amsmath,amsfonts}

\\title{" @ (skeleton-read "Title: " "") "}
\\author{" @ (skeleton-read "Author: " "Simon Zugmeyer") "}
\\date{}

%%------------------------------------------------------------------------------
%% Packages

\\usepackage{amsmath,amsfonts}
%% \\usepackage{amssymb}
%% \\usepackage{mathtools}
\\usepackage[matha,mathb]{mathabx} % french mathematical symbols and more
\\usepackage[integrals]{wasysym} % german integrals
\\usepackage{amsthm}
\\usepackage[]{geometry}

\\usepackage[french]{babel} % load last

%%------------------------------------------------------------------------------
%% Custom environments/macros

\\renewcommand{\\theenumi}{\\roman{enumi}} % roman numerals in enumerate

\\makeatletter
\\renewcommand\\d[1]{\\mspace{2mu}\\mathrm{d}#1\\@ifnextchar\\d{\\mspace{-1mu}}{}}
\\makeatother

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\\begin{document}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\\maketitle" \n
@ _ \n \n
"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\\end{document}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")

(provide 'mode-latex)
