
(defcustom masterfile nil
  "The file that should be compiled. Useful for modular documents."
  :safe 'stringp)

(defvar tex-index-command "makeindex"
  "The TeX index file generator.")

(defvar pdf-viewer "zathura" "PDF viewer.")

(defvar pdf-viewer-args
  '("--fork"
    "-x" "emacsclient --eval '(progn (switch-to-buffer  (file-name-nondirectory \"%{input}\")) (goto-line %{line}))'")
  "List of arguments passed to `pdf-viewer' when called.
You may want to fork the viewer so that it detects when the same
document is launched twice, and persists when Emacs gets closed.\n
For instance with `zathura':\n
  zathura --fork\n
We can use\n
  emacsclient --eval '(progn (switch-to-buffer  (file-name-nondirectory \"%{input}\")) (goto-line %{line}))'\n
to reverse-search a PDF using SyncTeX. Note this can only work
with emacsclient since we need to communicate the command to an
existing instance.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tex-set-compiler ()
  "Set `compile-command' for TeX-based document."
  (interactive)
  (hack-local-variables)
  (let* (;; Master file.
         (local-master
	  
          (if masterfile masterfile (if buffer-file-name buffer-file-name (error "Buffer has no file name"))))
         (dirname (file-name-directory local-master))
         (basename (file-name-sans-extension (file-name-nondirectory local-master)))
         ;; Note: makeindex fails with absolute file names, we need relative names.
         (idxfile ))
    (set (make-local-variable 'compile-command)
         (concat
          "cd " (if dirname dirname ".") " && "
;          (when (executable-find tex-index-command)
;            (concat tex-index-command " " (concat basename ".idx" ) " ; "))
;          (when (executable-find tex-bibtex-command)
;            (concat tex-bibtex-command " " basename " ; "))
          tex-command
          " " tex-start-options
          " " tex-start-commands
          " " (shell-quote-argument basename)))))

(defun pdf-view (&optional file)
  "Call `pdf-viewer' over FILE.
If FILE is not provided, use PDF associated to current buffer
filename. If called with universal argument, prompt for filename.
It FILE is not a PDF, the extension is automatically replaced by
.pdf."
  (interactive
   (list (if (equal current-prefix-arg '(4))
             (expand-file-name (read-file-name "PDF to view: " nil nil t
                                               (concat (file-name-base buffer-file-name) ".pdf"))))))
  (let ((pdf (concat
              (file-name-sans-extension
               (if file file
                 buffer-file-name))
              ".pdf")))
    (when (and
           (file-exists-p pdf) (file-writable-p pdf)
           (executable-find pdf-viewer))
      (apply 'start-process "dummy" nil pdf-viewer pdf pdf-viewer-args))))

(defun tex-pdf-view ()
  "Use `masterfile' variable as default value for `pdf-view'."
  (interactive)
  (hack-local-variables)
  (let ((local-master (if masterfile masterfile buffer-file-name)))
    (pdf-view local-master)))

(defun tex-clean ()
  "Remove all TeX temporary files. This command should be safe,
but there is no warranty."
  (interactive)
  (hack-local-variables)
  (let (
        ;; Master file.
        (local-master
         (if (not masterfile) buffer-file-name masterfile)))

    (let (
          ;; File name without extension.
          (file
           (replace-regexp-in-string "tex" "" (file-name-nondirectory local-master))))

      ;; Concatate file name to list.
      (mapcar
       ;; Delete file if exist
       (lambda (argfile) (interactive)
         (when (and (file-exists-p argfile) (file-writable-p argfile))
           (delete-file argfile)
           (message "[%s] deleted." argfile)))
       (mapcar
        ;; Concat file name with extensions.
        (lambda (arg) (interactive) (concat file arg))
        tex-extension-list)))))

(defun tex-pdf-compress ()
  "PDF compressions might really strip down the PDF size. The
compression depends on the fonts used. Do not use this command if
your document embeds raster graphics."
  (interactive)
  (hack-local-variables)
  (let (
        ;; Master file.
        (local-master
         (if (not masterfile) buffer-file-name masterfile)))

    (let (
          ;; Temp compressed file.
          (file-temp
           (concat (make-temp-name (concat "/tmp/" (file-name-nondirectory local-master))) ".pdf"))

          ;; File name with PDF extension.
          (file
           (replace-regexp-in-string "tex" "pdf" (file-name-nondirectory local-master))))

      (when (and (file-exists-p file) (file-writable-p file))
        (shell-command
         (concat  "gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=\"" file-temp "\" \"" file "\""))
        (rename-file file-temp file t)
        ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SETUP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq tex-start-options "-file-line-error-style -interaction=nonstopmode -synctex=1 ")
;; Use the following variable to append file local commands without erasing
;; default options.
(setq tex-start-commands nil)

(add-hook-and-eval
 'tex-mode-hook
 (lambda ()
   (local-set-key (kbd "<f9>") 'tex-pdf-view)
   (setq tex-command "pdftex")
   (add-hook 'compilation-before-hook 'tex-set-compiler nil t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'mode-tex)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
