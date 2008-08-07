(define-skeleton insert-double-brackets
  "Inserts double brackets, for zsh"
  ""
  > "[[ " _ " ]]")

(define-skeleton insert-braces
  "Insert braces"
  ""
  "{" \n
  _ \n
  (progn
    (beginning-of-line)
    (delete-char 2)
    )
  "}")

(define-skeleton electric-semi
  "Like electric semi"
  ""
  ";"\n
  _)

(define-skeleton semi-space
  "Space on semicolon"
  ""
  ": " _)

(setq skeleton-end-hook nil)

(provide 'my-skeletons)
