;; General

(global-set-key [(control tab)] 'next-multiframe-window)
(global-set-key [(control shift k)] 'kill-whole-line)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\C-j" 'newline)
(global-set-key [(control shift d)] 'duplicate-line)
(global-set-key "\C-xy" 'duplicate-line)
(global-set-key "\M-/" 'comment-region)
(global-set-key "\M-\C-m" 'insert-line-below)
(global-set-key [(shift enter)] 'newline-and-indent)
;;(global-set-key "\C-m" 'newline-and-indent)

;;(global-set-key "\M-0\M-/" 'uncomment-region)

;; Slime

;;(global-set-key "\C-p" 'slime-close-all-parens-in-sexp)


;; Web
 
(global-set-key (kbd "C-x w") 'w3m)
(global-set-key "\C-xh" 'view-url)
(global-set-key "\C-cT" 'twittering-update-status-interactive)


;; Hooks

(add-hook 'slime-repl-mode-hook
          (lambda ()
            (local-set-key [up] 'slime-repl-previous-input)
            (local-set-key [down] 'slime-repl-next-input)))

(add-hook 'ruby-mode-hook
          '(lambda ()
             ;; Default to auto-indent on Enter
             (local-set-key "\C-m" 'ruby-reindent-then-newline-and-indent)))

;; Markdown mode
(add-hook 'markdown-mode-hook
           '(lambda ()
              (define-key markdown-mode-map "\C-m" 'newline-and-indent)))

(provide 'my-bindings)
