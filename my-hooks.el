(defun set-skeleton-pairs (pairs)
  (mapcar '(lambda (pair)
             (local-set-key pair 'skeleton-pair-insert-maybe)) pairs)
  (setq skeleton-pair t)
  )

(defun lisp-keys ()
  "Keys shared between lisp mode and emacs lisp mode"
  (local-set-key [return] 'reindent-then-newline-and-indent)
  (local-set-key "\C-j" 'eval-print-last-sexp)

  (set-skeleton-pairs '("(" "{" "["))
  )

(add-hook 'slime-repl-mode-hook
          (lambda ()

            (local-set-key "\C-xp" 'slime-close-all-parens-in-sexp)
            (local-set-key [up] 'slime-repl-previous-input)
            (local-set-key [down] 'slime-repl-next-input)
            ))

(add-hook 'lisp-mode-hook
          '(lambda ()
             (textmate-mode 1)
             (lisp-keys)
             ))

(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (textmate-mode 1)
             (lisp-keys)
             ))

(add-hook 'ruby-mode-hook
          '(lambda ()
             (textmate-mode 1)

             (local-set-key [return] 'ruby-reindent-then-newline-and-indent)
             ))

(add-hook 'sh-mode-hook
          '(lambda ()
             (make-local-variable 'write-contents-hooks)
             (add-hook 'write-contents-hooks 'untabify-buffer)
             (setq sh-basic-offset 2)

             (local-set-key [return] 'newline-and-indent)
             (local-set-key (kbd "[") 'insert-double-brackets)
             ))

(add-hook 'python-mode-hook
          '(lambda ()
             (make-local-variable 'write-contents-hooks)
             (add-hook 'write-contents-hooks 'untabify-buffer)
             (setq python-indent 4)

             (local-set-key [return] 'newline-and-indent)
             ))

(add-hook 'css-mode-hook
          '(lambda ()
             (textmate-mode 1)
             (make-local-variable 'write-contents-hooks)
             (add-hook 'write-contents-hooks 'untabify-buffer)
             (setq css-indent-offset 2)

             (local-set-key [return] 'newline-and-indent)
             (local-set-key (kbd "{") 'insert-braces)
             (local-set-key (kbd ";") 'electric-semi)
             (local-set-key (kbd ":") 'semi-space)
             ))

(add-hook 'inferior-sml-load-hook
          '(lambda ()
             (load-library "sml-mosml")))

(add-hook 'markdown-mode-hook
          (lambda ()
            (textmate-mode 1)
            (auto-fill-mode 1)
            (setq tab-width 4)
            (make-local-variable 'write-contents-hooks)
            (add-hook 'write-contents-hooks 'untabify-buffer)))

(eval-after-load "sql"
  '(progn
     (sql-set-product 'postgresql)
     ))

(add-hook 'server-done-hook (lambda nil (kill-buffer nil)))
(add-hook 'write-file-hooks 'delete-trailing-whitespace)
(add-hook 'font-lock-mode-hook 'show-ws-highlight-tabs)

(provide 'my-hooks)
