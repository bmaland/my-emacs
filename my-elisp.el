(defun chomp (str)
  "Perl-like chomp function, trims whitespace"
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (save-excursion
      (while (and
              (not (null (string-match "^\\( \\|\f\\|\t\\|\n\\)" s)))
              (> (length s) (string-match "^\\( \\|\f\\|\t\\|\n\\)" s)))
        (setq s (replace-match "" t nil s)))
      (while (and
              (not (null (string-match "\\( \\|\f\\|\t\\|\n\\)$" s)))
              (> (length s) (string-match "\\( \\|\f\\|\t\\|\n\\)$" s)))
        (setq s (replace-match "" t nil s))))
    s))

(defun my-mark-word ()
  "Marks the whole word the cursor is placed on"
  (interactive)
  (backward-word)
  (mark-word))

;; TODO fix this, probably tramp issue
(defun find-alternative-file-with-sudo ()
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
             buffer-file-name))))

(defun my-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun my-print-macro-expansion ()
  "insert the expansion of a macro"
  (interactive)
  (backward-kill-sexp)
  (undo)
  (insert (concat "\n" (pp (cl-macroexpand (read (current-kill 0)))))))

;; TODO make this more general and use run-shell-command instead with sed etc
;; so the buffer doesn't get modified
(defun line-count-lisp ()
  (interactive)
  (save-excursion
    (flush-lines "^$")
    (flush-lines "^;")
    (goto-char (point-max))
    (let ((loc (line-number-at-pos)))
      (message (number-to-string loc) " lines of code. Be sure to undo now."))))

;; From Steve Yegge
(defun article-length ()
  "Print character and word stats on current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((char-count 0))
      (while (not (eobp))
        (unless (looking-at "[ \t\r\n]")
          (incf char-count))
        (forward-char 1))
      (message "%d chars, %d words" char-count (/ char-count 5)))))

(defun duplicate-line()
  "Duplicate the current line"
  (interactive)
  (let ((beg (line-beginning-position))
        (end (line-end-position))
        (column (current-column)))
    (copy-region-as-kill beg end)
    (end-of-line)
    (newline)
    (yank)
    (move-to-column column)))

(defun insert-line-below ()
  "Inserts a new line below cursor"
  (interactive)
  (end-of-line)
  (newline-and-indent))

;; Taken from http://emacs.wordpress.com/2007/01/22/killing-yanking-and-copying-lines/
(defun jao-copy-line ()
  "Copy current line in the kill ring"
  (interactive)
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position 2))
  (message "Line copied"))

(defun mark-line (&optional arg)
  "Marks a line from start of indentation to end"
  (interactive "p")
  (back-to-indentation)
  (end-of-line-mark arg))

(defun file2url ()
  "Uploads the file in the current buffer via file2url.sh, displays the resulting url."
  (interactive)
  (message "%s"
           (shell-command-to-string
            (concatenate 'string "file2url.sh " buffer-file-name))))

(defun indent-buffer ()
  "Indent whole buffer"
  (interactive)
  (save-excursion (indent-region (point-min) (point-max) nil)))

(defun untabify-buffer ()
  "Untabify the whole (accessible part of the) current buffer"
  (interactive)
  (save-excursion (untabify (point-min) (point-max))))

(defun dos2unix ()
  "Convert a buffer from dos ^M end of lines to unix end of lines"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

(defun load-elisp()
  "Automatic reload current file that major mode is Emacs-Lisp mode."
  (interactive)
  (if (member major-mode '(emacs-lisp-mode)) ;if current major mode is emacs-lisp-mode
      (progn
        (indent-buffer)                      ;format
        (save-buffer)                        ;save
        (byte-compile-file buffer-file-name) ;compile
        (load-file buffer-file-name)         ;loading
        (eval-buffer)                        ;revert
        )
    (message "Current major mode is not Emacs-Lisp mode, so not reload.") ;otherwise don't loading
    ))

(defun word-count ()
  "Count words in buffer"
  (interactive)
  (shell-command-on-region (point-min) (point-max) "wc -w"))

;; TODO this needs a lot of work
(defun word-freq ()
  "Count word frequency in buffer"
  (interactive)
  (shell-command-on-region (point-min) (point-max)
                           (concat
                            "tr ' ' '\n'|tr '[A-Z]' '[a-z]'|" ;; Split words and downcase
                            "sed 's/[()<>|/\"]//g'|"
                            "sed 's/[,\.;!\?:]$//'|"
                            "sed 's/^[^a-zA-Z]*//g'|"
                            "sort|grep -v '^$'|uniq -c|sort -nr|cat -b")))

(defun shell-here ()
  "Open a shell in `default-directory'."
  (interactive)
  (let ((dir (expand-file-name default-directory))
        (buf (or (get-buffer "*shell*") (shell))))
    (goto-char (point-max))
    (if (not (string= (buffer-name) "*shell*"))
        (switch-to-buffer-other-window buf))
    (message list-buffers-directory)
    (if (not (string= (expand-file-name list-buffers-directory) dir))
        (progn (comint-send-string (get-buffer-process buf)
                                   (concat "cd \"" dir "\"\r"))
               (setq list-buffers-directory dir)))))

;; Insert the date, the time, and the date and time at point.
(defvar insert-time-format "%T"
  "*Format for \\[insert-time] (c.f. 'format-time-string' for how to format).")

(defvar insert-date-format "%d.%m.%Y"
  "*Format for \\[insert-date] (c.f. 'format-time-string' for how to format).")

(defun insert-time ()
  "Insert the current time according to the variable \"insert-time-format\"."
  (interactive "*")
  (insert (format-time-string insert-time-format
                              (current-time))))

(defun insert-date ()
  "Insert the current date according to the variable \"insert-date-format\"."
  (interactive "*")
  (insert (format-time-string insert-date-format
                              (current-time))))

(defun insert-time-and-date ()
  "Insert the current date according to the variable \"insert-date-format\", then a space, then the current time according to the variable \"insert-time-format\"."
  (interactive "*")
  (progn
    (insert-date)
    (insert " ")
    (insert-time)))

(defun delete-empty-pair ()
  (defun is-empty-pair ()
    (let ((pairs '(( ?\( . ?\))
                   ( ?\' . ?\')
                   ( ?\" . ?\")
                   ( ?[ . ?])
                   ( ?{ . ?}))))
      (eq (cdr (assoc (char-before) pairs)) (char-after))))

  (interactive)
  (if (eq (char-after) nil)
      nil ;; if char-after is nil, just backspace
    (if (is-empty-pair)
        (delete-char 1)))
  (delete-backward-char 1))

;; TODO move over >
(defun move-over (char)
  "Move over ending pair characters, like in TextMate"
  (let ((pushovers
         '((?\" . (lambda () (forward-char 1)))
           (?\' . (lambda () (forward-char 1)))
           (?\| . (lambda () (forward-char 1)))
           (?\) . (lambda () (up-list 1)))
           (?\] . (lambda () (up-list 1)))
           (?\} . (lambda () (up-list 1)))
           ))
        (defaults
          '((?\" . (lambda () (skeleton-pair-insert-maybe nil)))
            (?\' . (lambda () (skeleton-pair-insert-maybe nil)))
            (?\| . (lambda () (skeleton-pair-insert-maybe nil)))
            (?\) . (lambda () (insert-char ?\) 1)))
            (?\] . (lambda () (insert-char ?\] 1)))
            (?\} . (lambda () (insert-char ?\} 1)))
            )))
    (if (eq (char-after) char)
        (funcall (cdr (assoc char pushovers)))
      (funcall (cdr (assoc char defaults))))))

(defun move-over-bracket () (interactive) (move-over ?\)))
(defun move-over-curly () (interactive) (move-over ?\}))
(defun move-over-square () (interactive) (move-over ?\]))
(defun move-over-quote () (interactive) (move-over ?\'))
(defun move-over-dbl-quote () (interactive) (move-over ?\"))
(defun move-over-pipe () (interactive) (move-over ?\|))

(defun set-pairs (pairs)
  "Sets up handling of pair characters."
  (mapcar '(lambda (pair)
             (local-set-key pair 'skeleton-pair-insert-maybe)
             (cond ((string= pair "\"") (local-set-key pair 'move-over-dbl-quote))
                   ((string= pair "\'") (local-set-key pair 'move-over-quote))
                   ((string= pair "|") (local-set-key pair 'move-over-pipe))
                   ((string= pair "[") (local-set-key "\]" 'move-over-square))
                   ((string= pair "(") (local-set-key "\)" 'move-over-bracket))
                   ((string= pair "{") (local-set-key "\}" 'move-over-curly))))
          pairs)
  (setq skeleton-pair t))

;; --------------------------------------------------------- ;;
;; Switch to buffer
(if (>= emacs-major-version 22)
    (progn
      (defun ignore-buffer (str)
        (or
         ;;buffers I don't want to switch to
         (string-match "\\*Buffer List\\*" str)
         (string-match "^TAGS" str)
         (string-match "^\\*Messages\\*$" str)
         (string-match "^\\*Completions\\*$" str)
         (string-match "^\\*scratch\\*$" str)
         (string-match "^\\*ESS\\*$" str)
         (string-match "^ " str)
         (string-match "Mew message" str)
         (string-match "output\\*$" str)
         (string-match "compilation" str)
         (string-match "^\\*TeX silent\\*$" str)
         ;;(string-match "inbox" str)
         ))

      (defun next-user-buffer ()
        "Switch to the next user buffer in cyclic order."
        (interactive)
        (next-buffer)
        (while (ignore-buffer (buffer-name))
          (next-buffer) ))

      (defun previous-user-buffer ()
        "Switch to the next user buffer in cyclic order."
        (interactive)
        (previous-buffer)
        (while (ignore-buffer (buffer-name))
          (previous-buffer)))))

(provide 'my-elisp)
