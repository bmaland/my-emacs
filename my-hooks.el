(defun lisp-hooks ()
  "Shared between lisp mode and emacs lisp mode"
  (defalias 'ms 'mark-sexp)

  (local-set-key [return] 'reindent-then-newline-and-indent)
  (local-set-key "\C-j" 'eval-print-last-sexp)
  (local-set-key "\M-." 'find-function)

  (local-set-key [(meta up)] 'beginning-of-defun)
  (local-set-key [(meta down)] 'end-of-defun)

  (set-pairs '("(" "{" "[" "\"")))

(add-hook 'slime-repl-mode-hook
          (lambda ()

            (local-set-key "\C-xp" 'slime-close-all-parens-in-sexp)
            (local-set-key [up] 'slime-repl-previous-input)
            (local-set-key [down] 'slime-repl-next-input)
            ))

(add-hook 'lisp-mode-hook
          '(lambda ()
             (lisp-hooks)
             ))

(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (lisp-hooks)

             (make-local-variable 'after-save-hook)
             (add-hook 'after-save-hook
                       ;; If you're saving an elisp file, likely the .elc is no longer valid.
                       (lambda ()
                         (if (file-exists-p (concat buffer-file-name "c"))
                             (delete-file (concat buffer-file-name "c")))))))

(add-hook 'ruby-mode-hook
          '(lambda ()
             (set-pairs '("(" "{" "[" "\"" "\'" "|"))

             (local-set-key [return] 'ruby-reindent-then-newline-and-indent)
             ))

(add-hook 'nxml-mode-hook
          '(lambda ()
             (set-pairs '("<" "{" "[" "\"" "\'"))

             (local-set-key [return] 'reindent-then-newline-and-indent)
             ))

(add-hook 'php-mode-hook
          '(lambda ()
             (set-pairs '("<" "{" "[" "\"" "\'"))
             (setq c-basic-offset 4)
             (c-set-offset 'inline-open 0)

             (local-set-key [return] 'reindent-then-newline-and-indent)
             ))

(add-hook 'sh-mode-hook
          '(lambda ()
             (set-pairs '("(" "{" "\"" "\'"))
             (setq sh-basic-offset 2)

             (local-set-key [return] 'reindent-then-newline-and-indent)
             (local-set-key "[" 'self-insert-command)
             ))

(add-hook 'python-mode-hook
          '(lambda ()
             (set-pairs '("(" "{" "[" "\"" "\'"))
             (setq python-indent 4)

             (local-set-key [return] 'reindent-then-newline-and-indent)
             ))

(add-hook 'css-mode-hook
          '(lambda ()
             (set-pairs '("(" "[" "\"" "\'"))

             (setq css-indent-offset 2)

             (local-set-key [return] 'newline-and-indent)
             (local-set-key (kbd "{") 'insert-braces)
             (local-set-key (kbd ";") 'electric-semi)
             (local-set-key (kbd ":") 'semi-space)
             ))

(add-hook 'inferior-sml-load-hook
          '(lambda ()
             (load-library "sml-mosml")))

(add-hook 'sml-mode-hook
          '(lambda ()
             (set-pairs '("(" "{" "[" "\"" "\'"))
             (setq sml-electric-semi-mode t)
             (local-set-key [return] 'reindent-then-newline-and-indent)))

(add-hook 'org-mode-hook
          (lambda ()
            (set-pairs '("(" "{" "[" "\""))
            (auto-fill-mode 1)))

(add-hook 'markdown-mode-hook
          (lambda ()
            (set-pairs '("(" "{" "[" "\""))
            (auto-fill-mode 1)
            (setq tab-width 4)))

(add-hook 'prolog-mode-hook
          (lambda ()
            (set-pairs '("(" "{" "[" "\"" "'"))
            ;;(modify-syntax-entry ?_ ".")
            (local-set-key [return] 'reindent-then-newline-and-indent)))

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("ruby" (mode . ruby-mode))
               ("haml" (mode . haml-mode))
               ("sass" (mode . sass-mode))
               ("shell" (mode . sh-mode))
               ("elisp" (mode . emacs-lisp-mode))
               ("gtalk" (mode . jabber-chat-mode))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

(add-hook 'server-done-hook (lambda nil (kill-buffer nil)))

(eval-after-load "sql"
  '(progn
     (sql-set-product 'postgresql)
     ))

(provide 'my-hooks)
