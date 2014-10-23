
(defun add-hook-and-eval (hook function)
  "Add FUNCTION to HOOK and evaluate it.
This can be useful when called from a hooked function to make
sure it gets executed."
  (add-hook hook function)
  (funcall function))

(defun comment-or-uncomment-current-line-or-region ()
  "Comments or uncomments current current line or whole lines in region."
  (interactive)
  (save-excursion
    (let (min max)
      (if (region-active-p)
          (setq min (region-beginning) max (region-end))
        (setq min (point) max (point)))
      (comment-or-uncomment-region
       (progn (goto-char min) (line-beginning-position))
       (progn (goto-char max) (line-end-position))))))
(define-key my-keys-minor-mode-map "\M-;" 'comment-or-uncomment-current-line-or-region)

(defcustom compilation-before-hook nil
  "List of hook functions run by `compile-custom'.
You may want to set the `compile-command' with this hook. If you
do so, do not forget to set the LOCAL flag to t."
  :type 'hook
  :group 'compilation)

(defvar compilation-time-before-hide-window nil
  "Hide compilation window after the specified seconds.
If nil, do not hide.")

(defun compile-custom (&optional runhooks)
  "Call `recompile'.
If RUNHOOKS is non-nil (or with universal argument), run hooks in
`compilation-before-hook', then `recompile', then
`compilation-after-hook'."
  (interactive "P")
  (when (or runhooks (string= compile-command "make -k ")) (run-hooks 'compilation-before-hook))
  (recompile)
  (when compilation-time-before-hide-window
    (sit-for compilation-time-before-hide-window)
    (delete-windows-on "*compilation*")))
  ;; (when (or runhooks (string= compile-command "make -k ")) (run-hooks 'compilation-after-hook)))

(defun rename-buffer-and-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))
(define-key my-keys-minor-mode-map (kbd "C-x w") 'rename-buffer-and-file)

(defun skeleton-make-markers ()
  "Save last skeleton markers in a list.
Hook function for skeletons."
  (while skeleton-markers
    (set-marker (pop skeleton-markers) nil))
  (setq skeleton-markers
        (mapcar 'copy-marker (reverse skeleton-positions))))

(defvar skeleton-markers nil
  "Markers for locations saved in skeleton-positions.")

(defun skeleton-next-position (&optional reverse)
  "Skeleton movements through placeholders."
  (interactive "P")
  (let ((positions (mapcar 'marker-position skeleton-markers))
        (comp (if reverse '< '<=))
        pos prev)
    (when positions
      (setq pos (pop positions))
      (while (and pos (funcall comp pos (point)))
        (setq prev pos)
        (setq pos (pop positions)))
      (cond
       ((and reverse prev) (goto-char prev))
       (reverse (goto-char (car (last skeleton-markers))))
       (pos (goto-char pos))
       (t (goto-char (car skeleton-markers)))))))

(provide 'functions)
