;; Extremely simple major mode for editing yasnippet definitions
;; By Bjørn Arild Mæland <bjorn.maeland at gmail.com>

(define-generic-mode 'yasnippet-mode
  '("#")
  '("name" "contributor" "if" "end" "else" "unless" "when" "condition" "each"
    "for" "true" "false" "case" "pattern")
  '(("${\\([a-zA-Z_]+\\)}" 1 font-lock-variable-name-face)
    ("[0-9]+" . 'font-lock-variable-name-face))
  nil
  (list (lambda ()
          (make-local-variable 'require-final-newline)
          (setq require-final-newline nil) ;; DONT require final newline
          ))
  "Major mode for editing yasnippet definitions.")

(provide 'yasnippet-mode)

;;; yasnippet-mode.el ends here
