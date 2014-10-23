;; Minimal UI
(setq inhibit-startup-screen t)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(menu-bar-mode -1)
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode 'left)
  (scroll-bar-mode -1)
  (define-key my-keys-minor-mode-map (kbd "C-<f6>") 'toggle-scroll-bar))

;; Place the auto-save-list in cache folder.
(setq auto-save-list-file-prefix emacs-cache-folder)

;; Remember last cursor position.
(require 'saveplace)
(setq save-place-file (concat emacs-cache-folder "saveplace"))
(setq-default save-place t)

;; Disable suspend key since it is useless on Emacs server.
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; Print column number in mode line.
(column-number-mode 1)

;; Kill whole line including \n.
(setq kill-whole-line t)

;; Indentation
(setq-default tab-width 3)
(setq-default standard-indent 3)
(setq-default indent-tabs-mode nil) ;; Indentation cannot insert tabs

;; Compilation bindings and conveniences.
(require 'functions)
(setq compilation-ask-about-save nil)
(autoload 'recompile "compile" nil t)
(define-key my-keys-minor-mode-map (kbd "<f10>") 'compile-custom)

;; Skeleton settings
(require 'functions)
;; Do not expand abbrevs in skeletons.
;(setq-default skeleton-further-elements '((abbrev-mode nil)))
(add-hook 'skeleton-end-hook 'skeleton-make-markers)
(define-key my-keys-minor-mode-map (kbd "C-<right>") 'skeleton-next-position)
(define-key my-keys-minor-mode-map (kbd "C-<left>") (lambda () (interactive) (skeleton-next-position t)))

;; Read Matlab files in Octave mode.
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

(provide 'main)
