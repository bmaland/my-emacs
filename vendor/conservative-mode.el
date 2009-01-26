;; Buffer local minor mode for working on other peoples source.
;; Disables intrusive reindentation and whitespace removal.

(define-minor-mode
  conservative-mode
  "Conservative mode"
  :lighter " Conservative"
  :keymap '(([return] . newline-and-indent))
  (kill-wspace-mode))

(provide 'conservative-mode)
