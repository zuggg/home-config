;; Start with abbrev minor mode on
(add-hook 
 'octave-mode-hook
 (lambda ()
   (abbrev-mode 1)))

;; Set comments to be '%' to be matlab-compatible.
(add-hook
 'octave-mode-hook
 (lambda ()
   (set (make-local-variable 'comment-start) "% ")))

(provide 'mode-octave)
