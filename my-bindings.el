;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Completion that uses many different methods to find options.
(global-set-key (kbd "M-/") 'hippie-expand)

;; Jump to a definition in the current file. (This is awesome.)
(global-set-key "\C-x\C-i" 'ido-imenu)

;; Movement for windows and buffers
(windmove-default-keybindings) ;; Shift+direction

(global-set-key [(control tab)]               'next-multiframe-window)
(global-set-key [(control shift iso-lefttab)] 'previous-multiframe-window)
(global-set-key [(control shift right)]       'next-user-buffer)
(global-set-key [(control shift left)]        'previous-user-buffer)

;;; WINDOW SPLITING
(global-set-key (kbd "M-0") 'delete-window) ; was digit-argument
(global-set-key (kbd "M-1") 'delete-other-windows) ; was digit-argument
(global-set-key (kbd "M-2") 'split-window-vertically) ; was digit-argument
(global-set-key (kbd "M-3") 'split-window-horizontally) ; was digit-argument
(global-set-key (kbd "M-o") 'other-window) ; was prefix

(global-set-key (kbd "<f9>") 'kill-this-buffer) ; Close file

;; File finding
(global-set-key (kbd "C-x M-f")   'ido-find-file-other-window)
(global-set-key (kbd "C-x C-M-f") 'find-file-in-project)
(global-set-key (kbd "C-x f")     'recentf-ido-find-file)
(global-set-key (kbd "C-x C-p")   'find-file-at-point)
(global-set-key (kbd "C-c y")     'bury-buffer)
(global-set-key (kbd "C-c r")     'revert-buffer)
(global-set-key (kbd "M-`")       'file-cache-minibuffer-complete)
(global-set-key (kbd "C-x C-b")   'ibuffer)
(global-set-key (kbd "C-x C-d")   'dired)

;; Use regex searches by default.
(global-set-key "\C-s"    'isearch-forward-regexp)
(global-set-key "\C-r"    'isearch-backward-regexp)
(global-set-key "\C-\M-s" 'isearch-forward)
(global-set-key "\C-\M-r" 'isearch-backward)

(global-set-key "\M-#"          'calc)
(global-set-key (kbd "C-x gd")  'google-define)
(global-set-key "\C-xf"         'recentf-ido-find-file)
(global-set-key "\M-`"          'ff-find-other-file)
(global-set-key "\C-c1"         'find-grep-dired)
(global-set-key "\C-c2"         'grep-find)
(global-set-key "\C-z"          'undo)
(global-set-key "\C-x\C-m"      'execute-extended-command)
(global-set-key "\C-c\C-m"      'execute-extended-command)
(global-set-key "\C-w"          'backward-kill-word)
(global-set-key "\C-cc"         'jao-copy-line)
(global-set-key "\C-\M-w"       'my-mark-word)
(global-set-key (kbd "M-@")     'my-mark-word)
(global-set-key (kbd "M-\"")    'select-text-in-quote)
(global-set-key (kbd "M-2")     'extend-selection)
(global-set-key (kbd "C-x l")   'downcase-word)
(global-set-key (kbd "C-x u")   'upcase-word)
(global-set-key (kbd "C-x c")   'capitalize-word)
(global-set-key "\C-x\C-k"      'kill-region)
(global-set-key "\C-c\C-k"      'kill-region)
(global-set-key "\C-j"          'newline)
(global-set-key (kbd "C-x C-r") 'find-alternative-file-with-su)
(global-set-key [f4]            'revert-buffer)
(global-set-key [f5]            'bookmark-set)
(global-set-key [f6]            'bookmark-jump)
(global-set-key (kbd "TAB")     'indent-according-to-mode)
(global-set-key [backspace]     'delete-empty-pair)
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "M-SPC")   'set-mark-command)
(global-set-key (kbd "C-x m")   'magit-status)
(global-set-key (kbd "s-s")     'sr-speedbar-toggle)

;; make cursor movement keys under right hand's home-row.
(global-set-key (kbd "M-j") 'backward-char) ; was indent-new-comment-line
(global-set-key (kbd "M-l") 'forward-char)
(global-set-key (kbd "M-i") 'previous-line) ; was tab-to-tab-stop
(global-set-key (kbd "M-k") 'next-line) ; was kill-sentence

;; Org
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map (kbd "C-c r") 'org-remember)

;; Quod Libet
(global-set-key [f10] 'quodlibet-prev)
(global-set-key [f11] 'quodlibet-next)
(global-set-key [f12] 'quodlibet-pause)

;; TextMate inspired bindings
(global-set-key (kbd "M-RET") 'insert-line-below)
(global-set-key (kbd "M-S-<return>") '(lambda ()
                                        (interactive)
                                        (previous-line)
                                        (insert-line-below)))
(global-set-key [(control shift k)] 'kill-whole-line)
(global-set-key [(control shift d)] 'duplicate-line)
(global-set-key [(control shift l)] 'mark-line)

(global-set-key "\C-xgf" 'textmate-goto-file)
(global-set-key "\C-xgs" 'textmate-goto-symbol)

(global-set-key "\C-cT" 'twittering-update-status-interactive)

(provide 'my-bindings)
