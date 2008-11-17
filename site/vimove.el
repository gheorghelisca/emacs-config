
(provide 'vimove)
(require 'viper)

(define-minor-mode vimove-mode
  "vimove-mode"
  nil
  "VM"
  '( ("h" . viper-backward-char)
     ("l" . viper-forward-char)
     ("k" . viper-previous-line)
     ("j" . viper-next-line)
     ("H" . windmove-left)
     ("J" . windmove-down)
     ("K" . windmove-up)
     ("L" . windmove-right)))

(defun vimove-mode ()
  (interactive)
  (unless (assoc 'vimove-mode minor-mode-map-alist)
    (add-to-list 'minor-mode-map-alist
                 (cons 'vimove-mode vimove-mode-map)))
  (setq vimove-mode (not vimove-mode)))

