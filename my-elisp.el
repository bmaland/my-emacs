(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             ;; Default to auto-indent on Enter
             (define-key emacs-lisp-mode-map "\C-m" 'reindent-then-newline-and-indent)))


(add-hook 'emacs-lisp-mode-hook
          ;; If you're saving an elisp file, likely the .elc is no longer valid.
          (lambda ()
            (make-local-variable 'after-save-hook)
            (add-hook 'after-save-hook
                      (lambda ()
                        (if (file-exists-p (concat buffer-file-name "c"))
                            (delete-file (concat buffer-file-name "c")))))))

(font-lock-add-keywords 'emacs-lisp-mode '(("(\\|)" . 'paren-face)))

(font-lock-add-keywords 'lisp-mode '(("(\\|)" . 'paren-face)))

(font-lock-add-keywords 'scheme-mode '(("(\\|)" . 'paren-face)))

(font-lock-add-keywords
 'emacs-lisp-mode
 '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
    1 font-lock-warning-face t)))

(defface paren-face
  '((((class color) (background dark))
     (:foreground "orange"))
    (((class color) (background light))
     (:foreground "orange")))
  "Face used to color parentheses."
  :group 'my-faces)


(defadvice indent-sexp (around indent-defun (&optional endpos))
  "Indent the enclosing defun (or top-level sexp)."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    ad-do-it))

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

(defun line-count-lisp ()
  (interactive)
  (save-excursion
    (flush-lines "^$")
    (flush-lines "^;")
    (goto-char (point-max))
    (let ((loc (line-number-at-pos)))
      (message (number-to-string loc) " lines of code. Be sure to undo now."))))

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))


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

(defun duplicate-line ()
  "Duplicates current line and inserts it below. TODO: keep position & work for regions as well"
  (interactive)
  (beginning-of-line)
  (kill-line)
  (yank)
  (newline)
  (yank)
  (end-of-line))

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

(defun untabify-buffer ()
  "Untabify the whole (accessible part of the) current buffer"
  (interactive)
  (save-excursion
    (untabify (point-min) (point-max))))

(defun dos2unix ()
  "Convert a buffer from dos ^M end of lines to unix end of lines"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

(defun indent-buffer ()
  (interactive)
  (save-excursion (indent-region (point-min) (point-max) nil)))

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


(provide 'my-elisp)
