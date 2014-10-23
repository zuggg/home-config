
(defvar my-keys-minor-mode-map (make-keymap)
  "Keymap for my-keys-minor-mode. See its docstring for more
details.")

(define-minor-mode my-keys-minor-mode
  "A minor mode so that all bindings assingned on the
my-keys-minor-mode-map override undesired major modes
bindings. We use a minor mode to override global keys. This is
also rather useful to list all personal global bindings: just
rgrep `my-keys-minor-mode-map' over `~/.emacs.d'.

Example: to assign some-function to C-i, use

  (define-key my-keys-minor-mode-map (kbd \"C-i\") 'some-function)"
  t " my-keys" 'my-keys-minor-mode-map)
(add-hook 'minibuffer-setup-hook (lambda () (my-keys-minor-mode 0) ) )

(defvar emacs-cache-folder "~/.cache/emacs/"
  "Cache folder is everything we do not want to track along with
  the configuration files.")
(if (not (file-directory-p emacs-cache-folder))
    (make-directory emacs-cache-folder t))

;; read config
(add-to-list 'load-path "~/.emacs.d/lisp")

;; load main config
(require 'functions nil t)
(require 'main nil t)

;; modes
(add-hook 'tex-mode-hook    (lambda () (require 'mode-tex)))
(add-hook 'latex-mode-hook  (lambda () (require 'mode-latex)))
(add-hook 'octave-mode-hook (lambda () (require 'mode-octave)))

;; use cache folder for package archives
(when (require 'package nil t)
  (setq package-user-dir (concat emacs-cache-folder "elpa"))
  (package-initialize))

;; key binding to activate uim (ex. C-\)
(define-key my-keys-minor-mode-map (kbd "M-p") 'uim-mode)
;; only load uim when switching input
(autoload 'uim-mode "uim-input" nil t)
