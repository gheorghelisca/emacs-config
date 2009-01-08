        
(setq viper-inhibit-startup-message 't)
(setq viper-expert-level '5)
(setq viper-ex-style-editing nil)  ; can backspace past start of insert / line
(setq viper-vi-style-in-minibuffer nil)

(add-to-list 'viper-emacs-state-mode-list
             'lisp-mode)
(add-to-list 'viper-emacs-state-mode-list
             inferior-lisp-mode-hook )

