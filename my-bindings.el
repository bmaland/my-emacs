;; General

(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key "\M-`" 'ff-find-other-file)
(global-set-key "\C-c1" 'find-grep-dired)
(global-set-key "\C-c2" 'grep-find)
(global-set-key "\C-z" 'undo)
(global-set-key [(control tab)] 'next-multiframe-window)
(global-set-key [(control shift iso-lefttab)] 'previous-multiframe-window)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-cc" 'jao-copy-line)
(global-set-key "\C-\M-w" 'my-mark-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\C-j" 'newline)
(global-set-key (kbd "C-x C-r") 'find-alternative-file-with-sudo)
(global-set-key [f4] 'revert-buffer)
(global-set-key [f5] 'bookmark-set)
(global-set-key [f6] 'bookmark-jump)
(global-set-key (kbd "TAB") 'indent-according-to-mode)
(global-set-key [backspace] 'delete-empty-pair)
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)

;; Org
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;; TextMate inspired bindings
(global-set-key (kbd "M-RET") 'insert-line-below)
(global-set-key (kbd "M-S-<return>") '(lambda ()
                                   (interactive)
                                   (previous-line)
                                   (insert-line-below)))
(global-set-key [(control shift k)] 'kill-whole-line)
(global-set-key [(control shift d)] 'duplicate-line)
(global-set-key [(control shift l)] 'mark-line)
(global-set-key "\M-l" 'goto-line)

;; TODO check if w3m is available first? or move to site file
(global-set-key (kbd "C-x w") 'w3m)
(global-set-key "\C-xh" 'view-url)
(global-set-key "\C-cT" 'twittering-update-status-interactive)

(provide 'my-bindings)
