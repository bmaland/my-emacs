;; Buffer local minor mode for working on other peoples source.

(define-minor-mode
  conservative-mode
  "Conservative mode"
  :lighter " Conservative"
  :keymap '(([return] . newline-and-indent)))

(provide 'conservative-mode)
