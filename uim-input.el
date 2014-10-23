;; read uim.el
(require 'uim)
;; uncomment next and comment out previous to load uim.el on-demand
;; (autoload 'uim-mode "uim" nil t)

;; set Hiragana input mode at activating uim.
(setq uim-default-im-prop '("action_anthy_utf8_hiragana"
                            "action_google-cgiapi-jp_hiragana"
                            "action_mozc_hiragana"))

;; set inline candidates displaying mode as default
(setq uim-candidate-display-inline t)

;; set UTF-8 as preferred character encoding (default is euc-jp).
(setq uim-lang-code-alist
      (cons '("Japanese" "Japanese" utf-8 "UTF-8")
            (delete (assoc "Japanese" uim-lang-code-alist) 
                   uim-lang-code-alist)))
