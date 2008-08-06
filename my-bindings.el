;; General

(global-set-key "\M-`" 'ff-find-other-file)
(global-set-key "\C-c1" 'find-grep-dired)
(global-set-key "\C-c2" 'grep-find)
(global-set-key "\C-z" 'undo)
(global-set-key [(control tab)] 'next-multiframe-window)
(global-set-key [(control shift k)] 'kill-whole-line)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\C-j" 'newline)
(global-set-key [(control shift d)] 'duplicate-line)
(global-set-key "\M-\C-m" 'insert-line-below)
(global-set-key [(control shift l)] 'mark-line)
(global-set-key (kbd "C-x C-r") 'find-alternative-file-with-sudo)
(global-set-key [f5] 'bookmark-set)
(global-set-key [f6] 'bookmark-jump)
(global-set-key (kbd "TAB") 'indent-according-to-mode)

;; TODO check if w3m is available first? or move to site file
(global-set-key (kbd "C-x w") 'w3m)
(global-set-key "\C-xh" 'view-url)
(global-set-key "\C-cT" 'twittering-update-status-interactive)

;; Hooks
(add-hook 'slime-repl-mode-hook
          (lambda ()
            (local-set-key "\C-xp" 'slime-close-all-parens-in-sexp)
            (local-set-key [up] 'slime-repl-previous-input)
            (local-set-key [down] 'slime-repl-next-input)
            ))

(add-hook 'ruby-mode-hook
          '(lambda ()
             (local-set-key "\C-m" 'ruby-reindent-then-newline-and-indent)
             ))

(add-hook 'python-mode-hook
          '(lambda ()
             (local-set-key "\C-m" 'newline-and-indent)
             ))

(add-hook 'sh-mode-hook
          '(lambda ()
             (local-set-key "\C-m" 'newline-and-indent)
             (local-set-key (kbd "[") 'insert-double-brackets)
             ))

(add-hook 'css-mode-hook
          '(lambda ()
             (local-set-key (kbd "<return>") 'newline-and-indent)
             (local-set-key (kbd "{") 'insert-braces)
             (local-set-key (kbd ";") 'electric-semi)
             ))

(add-hook 'lisp-mode-hook
          '(lambda ()
             (local-set-key "\C-m" 'reindent-then-newline-and-indent)
             (local-set-key (kbd "TAB") 'lisp-indent-line)
             (define-key lisp-mode-map "\C-j" 'eval-print-last-sexp)
             ))

(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (local-set-key "\C-m" 'reindent-then-newline-and-indent)
             (local-set-key (kbd "TAB") 'lisp-indent-line)
             (define-key lisp-mode-map "\C-j" 'eval-print-last-sexp)
             ))

;; Markdown mode
;;(add-hook 'markdown-mode-hook
;;          '(lambda ()))

(provide 'my-bindings)
