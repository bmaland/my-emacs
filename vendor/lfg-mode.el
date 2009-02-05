;;;; lfg-mode.el --- LFG mode commands.

;; Author: Mary Dalrymple <dalrympl@parc.xerox.com>, Nobi Broeker
;; <nobi@ims.uni-stuttgart.de>, Jonas Kuhn <kuhn@ims.uni-stuttgart.de>
;; Keywords: lfg xle

;; This file is not a part of GNU Emacs, but is made available under
;; the same conditions.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; Use menus for lexical items and rules.  This is now accomplished by
;; the file imenu-go, which is appended verbatim below.  imenu-go
;; requires the library package imenu.
(eval-when-compile (require 'cl))
;; Run an XLE subprocess.
(require 'comint)

;;;###autoload
(defun lfg-mode ()
  "Major mode for editing LFG files.
   Provides menus of lexical items, rules, and templates with the
   imenu command.
   You can send a line of text to an inferior XLE process
   from buffers in lfg-mode by using the command
   \\[xle-parse-input-line], bound to M-C-p.
   You can send a paragraph of text to an inferior XLE process
   from buffers in lfg-mode by using the command
   \\[xle-parse-input], bound to M-C-x.
   You can reformat a rule, template, or lexical item by using 
   the command \\[lfg-format-expression], bound to M-q.
   You can create a display of a rule, template, or lexical item 
   with the comments removed by using the command 
   \\[lfg-display-expression], bound to M-C-d."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'lfg-mode)
  (setq mode-name "LFG")
  (lfg-mode-variables)
  (use-local-map lfg-mode-map)
  (set-syntax-table lfg-mode-syntax-table))

;;;###autoload
(defalias 'LFG-mode 'lfg-mode)

;;; Automatically loaded for files ending in ".lfg".
(setq auto-mode-alist
  (append '(("\\.lfg" . lfg-mode)) auto-mode-alist))

;;; Key mappings
(defvar lfg-mode-map (make-sparse-keymap)
  "Keymap used in LFG mode.")

; Parse some input
(define-key lfg-mode-map "\e\C-x" 'xle-parse-input)
; Parse a line of input
(define-key lfg-mode-map "\e\C-p" 'xle-parse-input-line)
; Regenerate a line of input
(define-key lfg-mode-map "\e\C-r" 'xle-regenerate-line)
; Display a template, rule, or lex item in another window with no comments
(define-key lfg-mode-map "\e\C-d" 'lfg-display-expression)
; start XLE from LFG mode buffer (used C-c C-f which is compile-file in latex-mode)
(define-key lfg-mode-map "\C-c\C-f" 'run-xle) 
; (un)comment region
(define-key lfg-mode-map "\C-c\C-c" 'lfg-comment-region)

(global-set-key [f9] 'move-focus-to-xle)

;;; Extended imenu-go functions
(global-set-key "\e\"" 'imenu-go-find-at-position)
(global-set-key [?\C-\"] 'imenu-go--back)

;;; Menu items
(define-key lfg-mode-map [menu-bar lfg]
  (cons "LFG" (make-sparse-keymap "LFG")))
(define-key lfg-mode-map [menu-bar lfg lfg-display-expression]
  '("Display a rule, template, or lexical item with comments removed" . lfg-display-expression))
(define-key lfg-mode-map [menu-bar lfg lfg-display-comments-all]
  '("Display comments from .lfg files in current directory" . lfg-display-comments-all))
(define-key lfg-mode-map [menu-bar lfg lfg-display-comments-config]
  '("Display comments from .lfg files referenced in config information" . lfg-display-comments-config))
(define-key lfg-mode-map [menu-bar lfg lfg-display-comments]
  '("Display comments from current file" . lfg-display-comments))
(define-key lfg-mode-map [menu-bar lfg xle-run-compare-full]
  '("Compare two XLE runs, full report" . xle-run-compare-full))
(define-key lfg-mode-map [menu-bar lfg xle-run-compare]
  '("Compare two XLE runs, report changes regarding which strings parse" . xle-run-compare))
(define-key lfg-mode-map [menu-bar lfg lfg-list-file-contents]
  '("Write menu contents to a file" . lfg-list-file-contents))
(define-key lfg-mode-map [menu-bar lfg imenu]
  '("Rules, templates, lexicon menus" . imenu))
(define-key lfg-mode-map [menu-bar lfg run-xle]
  '("Start an XLE process in *XLE* buffer, or switch to existing one" . run-xle))
(define-key lfg-mode-map [menu-bar lfg run-new-xle]
  '("Start a *new* XLE process" . run-new-xle))

;;; Character syntax
(defvar lfg-mode-syntax-table nil
  "Syntax table in use in lfg-mode buffers.")

(if lfg-mode-syntax-table
    ()
  (setq lfg-mode-syntax-table (make-syntax-table))
    (modify-syntax-entry ?\" "\"" lfg-mode-syntax-table)
    (modify-syntax-entry ?% "w" lfg-mode-syntax-table)
    (modify-syntax-entry ?\' "w" lfg-mode-syntax-table)
    (modify-syntax-entry ?- "w" lfg-mode-syntax-table)
    (modify-syntax-entry ?^ "w" lfg-mode-syntax-table)
    (modify-syntax-entry ?_ "w" lfg-mode-syntax-table)
    (modify-syntax-entry ?~ "w" lfg-mode-syntax-table)
    (modify-syntax-entry ?: "w" lfg-mode-syntax-table)
    (modify-syntax-entry ?; "w" lfg-mode-syntax-table)
    (modify-syntax-entry ?! "w" lfg-mode-syntax-table)
    (modify-syntax-entry ?? "w" lfg-mode-syntax-table)
    (modify-syntax-entry ?, "w" lfg-mode-syntax-table)
    (modify-syntax-entry ?@ "w" lfg-mode-syntax-table)
    (modify-syntax-entry ?\( "()  " lfg-mode-syntax-table)
    (modify-syntax-entry ?\) ")(  " lfg-mode-syntax-table)
    (modify-syntax-entry ?\{ "(}  " lfg-mode-syntax-table)
    (modify-syntax-entry ?\} "){  " lfg-mode-syntax-table)
    (modify-syntax-entry ?\[ "(]  " lfg-mode-syntax-table)
    (modify-syntax-entry ?\] ")[  " lfg-mode-syntax-table))


; Default is all colors; set to 0 if you want color only for comments
(defvar lfg-color-level t)

(defvar lfg-font-lock-keywords-0
    (list
     ;; Hash mark is comment character if at beginning of line.
     (cons "^[ \t]*\#\.*$" 'font-lock-comment-face))
  "Expressions to highlight in Level 0 highlighting, LFG mode."
  )

(defvar lfg-font-lock-keywords-1
   (list
     ; Highlight category on lhs of template
     (list "^[ \t]*\\([A-Za-z][^:^\n)(]+\\(?:([^)^]+)\\)?\\)[ \t]*=" 1 'font-lock-keyword-face)
     ; Highlight headword in lexical entry
     (list "^[ \t]*\\(.+?\\)[ \t]+[^ \n\t]+?[ \t]+\\(\\*\\|XLE\\)" 1 'font-lock-keyword-face)
     ; Highlight category on lhs of rule
     (list "^\\([^^\n]+\\)[ \t]*-->" 1 'font-lock-keyword-face)
     ; Highlight at signs
     (cons "@" 'font-lock-builtin-face)
     ; Highlight open and close brackets 
     (cons "[}{|]" 'font-lock-builtin-face))
   "Expressions to highlight in Level 1 highlighting, LFG mode."
   )

(defvar lfg-more-colors t 
   "If t, makes everything more colorful")

(if lfg-more-colors
   ; Gives brighter colors.
   (add-hook 'lfg-mode-hook 
     (function (lambda () 
        (set-face-foreground font-lock-comment-face "Chocolate")
        (set-face-foreground font-lock-string-face "Chocolate")
        (set-face-foreground font-lock-keyword-face "Purple")
        (set-face-foreground font-lock-builtin-face "Red")))))

;;; Variables for the mode
(defun lfg-mode-variables ()
  ;; Turn on fonts in different colors.
  (add-hook 'lfg-mode-hook 'turn-on-font-lock)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults 
	'((lfg-font-lock-keywords-0 lfg-font-lock-keywords-1)  nil nil ((?_ . "w"))))
  (make-local-variable 'font-lock-maximum-decoration)
  (setq font-lock-maximum-decoration lfg-color-level)
  ;; Blink matching parentheses.
  (make-local-variable 'blink-matching-paren)
  (setq blink-matching-paren t)
  ;; Sort rules and lexical items alphabetically, unless
  ;; 'lfg-no-imenu-sort-function is set to t.
  (or (boundp 'lfg-no-imenu-sort-function)
      (and (make-local-variable 'imenu-sort-function)
           (setq imenu-sort-function 'imenu--sort-by-name)))
  ;; Use the right sort function to create menu items.
  (make-local-variable 'imenu-create-index-function)
  (setq imenu-create-index-function 'lfg-create-index)
  ;; Turn off auto-fill.
  (make-local-variable 'auto-fill-function)
  (setq auto-fill-function nil)
  ;; Variables for filling and formatting.
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'fill-paragraph-function)
  (setq fill-paragraph-function 'lfg-format-expression)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  ; Frames for expression display.
  (make-local-variable 'special-display-regexps)
  (setq special-display-regexps '("\\*LFG File Display"))
  (run-hooks 'lfg-mode-hook))

;;;;;;;;; Run an inferior XLE process and interact with it.

;;; Customizable variable for font coding system.
(defvar xle-buffer-process-coding-system 'iso-latin-1 
           "Default coding system for sending text to XLE process.")

;;; Use xle-buffer-process-coding-system for all xle buffers.
(modify-coding-system-alist 'process "xle" xle-buffer-process-coding-system)

;;;###autoload
(defun inferior-xle-mode ()
  "Major mode for interacting with an inferior XLE process.
   You can send a line of text to the inferior XLE process
   from other LFG buffers by using the command
   \\[xle-parse-input-line], bound to M-C-p.
   You can send a paragraph of text to the inferior XLE process
   from other LFG buffers by using the command
   \\[xle-parse-input], bound to M-C-x.
   You can regenerate from a string by using the command
   \\[xle-regenerate-line], bound to M-C-r."
  (interactive)
  (comint-mode)
  (setq comint-prompt-regexp "% ")
  (setq comint-scroll-show-maximum-output t)
  (setq major-mode 'inferior-xle-mode)
  (use-local-map xle-mode-map)
  (setq mode-name "XLE"))

;;; Menu items
(defvar xle-mode-map nil)
(cond ((not xle-mode-map)
       (setq xle-mode-map (nconc (make-sparse-keymap)
				 comint-mode-map))
       (define-key xle-mode-map [menu-bar xle]
              (cons "XLE" (make-sparse-keymap "XLE")))
       (define-key xle-mode-map [menu-bar xle xle-generate-all-from-file]
             '("Generate from file" . xle-generate-all-from-file))
       (define-key xle-mode-map [menu-bar xle xle-parse-testfile]
             '("Parse a testfile" . xle-parse-testfile))
       (define-key xle-mode-map [menu-bar xle xle-create-generator]
             '("Create a generator" . xle-create-generator))
       (define-key xle-mode-map [menu-bar xle xle-create-parser]
             '("Create a parser" . xle-create-parser))
       (define-key xle-mode-map [menu-bar xle run-new-xle]
             '("Start a *new* XLE process in another window" . run-new-xle))
       (define-key xle-mode-map [menu-bar xle xle-restart]
             '("Restart XLE" . xle-restart))
       ; restart XLE from XLE buffer (used C-c C-f which is compile-file in latex-mode)
       (define-key xle-mode-map "\C-c\C-f" 'xle-restart)))

(defun run-xle ()
  "Run an inferior XLE process, input and output via buffer *XLE*."
  (interactive)
  (switch-to-buffer (make-comint "XLE" "xle"))
  (inferior-xle-mode))

(defun run-new-xle ()
  "Run a NEW inferior XLE process, even if one is already running,
  input and output via buffer *XLE*."
  (interactive)
  (switch-to-buffer (create-file-buffer "*XLE*"))
  (comint-exec (current-buffer) "XLE" "xle" nil nil)
  (inferior-xle-mode))

(defun xle-restart ()
   "Restart the XLE process from the XLE window.  Move point to the
    end of the buffer."
  (interactive)
  (comint-exec (current-buffer) "XLE" "xle" nil nil)
  ;; to end up at eob
  (goto-char (point-max))
  ;; nobi@ims 15Apr98
  )

(defvar lfg-default-parser nil "User-settable name of the default parser")

(defun xle-create-parser ()
   "Create a parser by reading in grammar and lexicon files."
  (interactive)
  (let ((xle-command (concat "create-parser "
                        (read-from-minibuffer "Name of parser to create: " lfg-default-parser))))
       (send-string (current-buffer) (concat "puts \{" xle-command "\}"))
       (send-string (current-buffer) "\n")
       (send-string (current-buffer) xle-command)
       (send-string (current-buffer) "\n")))

(defun xle-parse-testfile ()
   "Parse a testfile."
  (interactive)
  (let ((xle-command (concat "parse-testfile "
                        (read-from-minibuffer "Name of testfile: ")
                        " " 
                        (read-from-minibuffer "From sentence no.: " "1")
                        " " 
                        (read-from-minibuffer "To sentence no.: " "end"))))
       (send-string (current-buffer) (concat "puts \{" xle-command "\}"))
       (send-string (current-buffer) "\n")
       (send-string (current-buffer) xle-command)
       (send-string (current-buffer) "\n")))

(defun xle-create-generator ()
   "Create a generator by reading in grammar and lexicon files."
  (interactive)
  (let ((xle-command (concat "create-generator "
                        (read-from-minibuffer "Name of generator to create: ")
                        " \"" 
                        (read-from-minibuffer "Config section -- e.g. (ENGLISH GRAM): " "default")
                        "\" \"" 
                        (read-from-minibuffer "Addmode (addonly/addany/dontadd): " 
                                          "addonly") 
                        "\"")))
       (send-string (current-buffer) (concat "puts \{" xle-command "\}"))
       (send-string (current-buffer) "\n")
       (send-string (current-buffer) xle-command)
       (send-string (current-buffer) "\n")))

(defun xle-generate-all-from-file ()
   "Generate from a file."
  (interactive)
  (let ((xle-command (concat "generate-all-from-file "
                        (read-from-minibuffer "Name of input file: ")
                        " " 
                        (read-from-minibuffer "From sentence no.: " "1")
                        " " 
                        (read-from-minibuffer "To sentence no.: " "end"))))
       (send-string (current-buffer) (concat "puts \{" xle-command "\}"))
       (send-string (current-buffer) "\n")
       (send-string (current-buffer) xle-command)
       (send-string (current-buffer) "\n")))

(defun xle-parse-input-line (&optional buffer-number)
  "Send the current line of a file in LFG Mode to the XLE process running
   in the buffer *XLE*, or in buffer *gud-xle*, or in buffer
   *XLE*<arg> if called with a numerical argument, or in a new buffer
   *XLE* if none of these exists .  Put the line in curly brackets and
   preface with  `parse'.  Also send a command to display the line that
   is being parsed. Start an xle process if there is not one already."
  (interactive "p")
  (let ((home (current-buffer))
        (xle-buffer (xle-create-buffer-name buffer-number)))
  (cond ((get-buffer xle-buffer)
         t)
        ((get-buffer "*gud-xle*")
         (setq xle-buffer "*gud-xle*"))
        (t
         (progn (switch-to-buffer (create-file-buffer xle-buffer))
                (comint-exec (current-buffer) "XLE" "xle" nil nil)
                (inferior-xle-mode)
                (switch-to-buffer home))))
  (display-buffer xle-buffer t)
  (save-excursion
    (beginning-of-line)
    (let ((beg (point))
          string-to-parse)
      (end-of-line)
      (setq string-to-parse (concat "parse \{" 
                                    (buffer-substring beg (point))
				     "\}"))
      (send-string xle-buffer (concat "puts \{" string-to-parse "\}"))
      (send-string xle-buffer "\n")
      (send-string xle-buffer string-to-parse)
      (send-string xle-buffer "\n")))))

(defun xle-parse-input (&optional buffer-number)
  "Send the current paragraph a file in LFG Mode to the XLE process running
   in the buffer *XLE*, or in buffer *gud-xle*, or in buffer
   *XLE*<arg> if called with a numerical argument, or in a new buffer
   *XLE* if none of these exists.  Put the line in curly brackets and
   preface with `parse'.  Also send a command to display the line that
   is being parsed. Start an xle process if there is not one already."
  (interactive "p")
  (let ((home (current-buffer))
        (xle-buffer (xle-create-buffer-name buffer-number)))
  (cond ((get-buffer xle-buffer)
         t)
        ((get-buffer "*gud-xle*")
         (setq xle-buffer "*gud-xle*"))
        (t
         (progn (switch-to-buffer (create-file-buffer xle-buffer))
                (comint-exec (current-buffer) "XLE" "xle" nil nil)
                 (inferior-xle-mode)
                 (switch-to-buffer home))))
  (display-buffer xle-buffer t)
  (save-excursion
    (backward-paragraph)
    (let ((beg (point))
          string-to-parse)
      (forward-paragraph)
      (setq string-to-parse
              (concat "parse \{"
                      (xle-cleanup-input beg (point))
		      "\}"))
      (send-string xle-buffer (concat "puts \{" string-to-parse "\}"))
      (send-string xle-buffer "\n")
      (send-string xle-buffer string-to-parse)
      (send-string xle-buffer "\n")))))

(defun xle-regenerate-line (&optional buffer-number)
  "Send the current line of a file in LFG Mode to the XLE process running
   in the buffer *XLE*, or in buffer *gud-xle*, or in buffer
   *XLE*<arg> if called with a numerical argument, or in a new buffer
   *XLE* if none of these exists .  Put the line in curly brackets and
   preface with  `regenerate'.  Also send a command to display the line that
   is being regenerated. Start an xle process if there is not one already."
  (interactive "p")
  (let ((home (current-buffer))
        (xle-buffer (xle-create-buffer-name buffer-number)))
  (cond ((get-buffer xle-buffer)
         t)
        ((get-buffer "*gud-xle*")
         (setq xle-buffer "*gud-xle*"))
        (t
         (progn (switch-to-buffer (create-file-buffer xle-buffer))
                (comint-exec (current-buffer) "XLE" "xle" nil nil)
                (inferior-xle-mode)
                (switch-to-buffer home))))
  (display-buffer xle-buffer t)
  (save-excursion
    (beginning-of-line)
    (let ((beg (point))
          string-to-parse)
      (end-of-line)
      (setq string-to-parse (concat "regenerate \{" 
                                    (buffer-substring beg (point))
				     "\}"))
      (send-string xle-buffer (concat "puts \{" string-to-parse "\}"))
      (send-string xle-buffer "\n")
      (send-string xle-buffer string-to-parse)
      (send-string xle-buffer "\n")))))

(defun move-focus-to-xle ()
  (interactive)
  (send-string (current-buffer) "next-xle-focus\n"))

(defun xle-create-buffer-name (buffer-number)
   "Construct a name for the XLE buffer."
   (let ((buffer-name "*XLE*"))
     (if (= 1 buffer-number)
         buffer-name
         (concat buffer-name "<" (int-to-string buffer-number) ">"))))

(defun xle-cleanup-input (beg end)
  "Turn the text from beg to end into a string: remove carriage
   returns and extra spaces." 
   (let ((parse-buffer (generate-new-buffer "xle-parse-input"))
         input-string
         input-beg)
     (append-to-buffer parse-buffer beg end)
     (set-buffer parse-buffer)
     (goto-char (point-min))
     (skip-chars-forward " \n\t")
     (setq input-beg (point))
     (while (search-forward "\n" nil t)
            (replace-match " " nil t))
     (goto-char (point-max))
     (skip-chars-backward " \n\t")
     (setq input-string (buffer-substring input-beg (point)))
     (kill-buffer parse-buffer)
     input-string))

;;;;;;;;; Dynamically created menus of rules and lexical items.
;;;;;;;;; Uses imenu.el.

(defun lfg-create-index ()
  "Create an index of rules, templates, lexical items, and rule
  macros.  Uses imenu.el."
  (save-excursion
   (let (beg-block
         end-block
         config-name-end
         config-name
         (index-alist-block '())
         (index-alist-block2 '())
         (index-alist '()))
    (goto-char (point-min))
    (message "Scanning ...")
    (save-match-data
    (while (re-search-forward "\\(RULES\\|TEMPLATES\\|LEXICON\\)[ \n\t]+([0-9]+.[0-9]+)" nil t)
           ; Get the name of the configuration.
           (goto-char (setq beg-block (match-beginning 0)))
           (skip-chars-backward " \t\n")
           (setq config-name-end (point))
           (forward-word -1)
           (setq config-name (buffer-substring (point) config-name-end))
           (skip-chars-backward " \t\n")
           (setq config-name-end (point))
           (forward-word -1)
           (setq config-name
                 (cons (buffer-substring (point) config-name-end) config-name))
           (cond ((re-search-forward "^[ ]*----[^-]" nil t)
                  (setq end-block (match-beginning 0)))
                 (t
                  (setq end-block (point-max))))
           (goto-char beg-block)
           ; Make the alist of menu items.
           (cond
              ((looking-at "RULES[ \n\t]+\([0-9]+.[0-9]+\)")
               (goto-char (match-end 0))
               (while (lfg-forward-item end-block)
                      (cond ((lfg-is-rule)
                             (push (lfg-example--name-and-position "[ \n\t]*-->")
		                   index-alist-block))
                             ((lfg-is-macro)
                              (push (lfg-example--name-and-position "[ \n\t]*=")
		                    index-alist-block2)))
                      (lfg-goto-end-def end-block))
               (push (cons (concat (car config-name) " " (cdr config-name) " Rules")
                       index-alist-block)
                     index-alist)
               (push (cons (concat (car config-name) " " (cdr config-name) " Rule macros")
                       index-alist-block2)
                     index-alist)
               (setq index-alist-block '())
               (setq index-alist-block2 '())
               (goto-char end-block))
              ((looking-at "TEMPLATES[ \n\t]+\([0-9]+.[0-9]+\)")
               (goto-char (match-end 0))
               (while (lfg-forward-item end-block)
                      (push (lfg-example--name-and-position "[ \n\t]*=")
                                 index-alist-block)
                      (lfg-goto-end-def end-block))
               (push (cons (concat (car config-name) " " (cdr config-name) " Templates")
                       index-alist-block)
                     index-alist)
               (setq index-alist-block '())
               (goto-char end-block))
              ((looking-at "LEXICON[ \n\t]+\([0-9]+.[0-9]+\)")
               (goto-char (match-end 0))
               (while (lfg-forward-item end-block)
                      (push (lfg-example--name-and-position "[ \n\t]+")
                                 index-alist-block)
                      (lfg-goto-end-def end-block))
               (push (cons (concat (car config-name) " " (cdr config-name) " Lexicon")
                       index-alist-block)
                     index-alist)
               (setq index-alist-block '())
               (goto-char end-block)))))
    (message "")
    index-alist)))

(defun lfg-is-rule ()
   "We are looking at a rule (not a rule macro).  Rules can have
    arguments in square brackets."
    (looking-at "[^ \n\t]+\\(\\[[^]]+\\]\\)?[ \n\t]*-->"))

(defun lfg-is-macro ()
   "We are looking at a rule macro (not a rule).  Rule macros can have
    arguments in parentheses."
    (looking-at "[^ \n\t]+\\(([^)]+)\\)?[ \n\t]*="))

(defun lfg-forward-item (end-block)
   "Find an occurrence of a name of a template, lexical item, rule, or
    rule macro beginning after point but not extending past end-block.
    Set point at the beginning of the item, and return the string
    corresponding to the item."
   (let (beg word)
    (skip-chars-forward " \t\n" end-block)
    (lfg-skip-comments)
    (setq beg (point))
    ; Parenthesis or curly bracket might actually begin a lexical item.
    (cond 
        ((looking-at "\(")
         (skip-chars-forward "^\}\{ \t\n" end-block))
        ((looking-at "\{")
         (skip-chars-forward "^(\} \t\n" end-block))
        (t
         (skip-chars-forward "^(\}\{ \[\t\n") end-block))
    (setq word (buffer-substring-no-properties beg (point)))
    (if (and (< (point) end-block)
             (< beg (point)))
        (and (goto-char beg)
             word))))

(defun lfg-example--name-and-position (stringtype)
  "Modified from imenu-example--name-and-position.  Returns the
   lexical item, rule name, or template name after point and the marker
   where it begins."
   (let ((beg (point))
	 (marker (make-marker)))
     (skip-chars-forward "^ \t\n")
   (forward-char -1)
   ; Skip over commented characters in word or whatever it is.
   (while (looking-at "[`]")
          (forward-char 2)
          (skip-chars-forward "^ \t\n")
          (forward-char -1))
   (forward-char 1)
   ; Skip to end of item as defined by stringtype.  E.g. for a rule
   ; stringtype will be "-->".
   (re-search-forward stringtype)
   (goto-char (match-beginning 0))
   (set-marker marker (point))
   (cons (buffer-substring beg (point)) marker)))

(defun lfg-skip-comments ()
   "Skip over any comments between entries."
   (if (looking-at "\"")
       (progn (forward-char 1)
              (search-forward "\"" nil t)
              (skip-chars-forward " \t\n")
              (lfg-skip-comments))
       t))

(defun lfg-goto-end-def (end-block)
   "Find the end of the template or lexical item definition that point
    is currently in.  In general this will be the next period, but
    watch out for comments.  Don't go past end-block.  Assumes we are
    NOT currently in a comment."
   (re-search-forward "[^`]\\(\"\\|\\.\\)" end-block)
   (forward-char -1)
   (while (looking-at "\"")
          (forward-char 1)
          (re-search-forward "\"" end-block)
          (re-search-forward "\\(\"\\|\\.\\)" end-block)
          (forward-char -1))
   (if (not (>= (point) end-block))
       (progn (forward-char 1)
              (point))))

;;; Dump contents of menus to a file.

(defun lfg-list-file-contents ()
   "Dumps the contents of the menu for a file to the file *Contents of file*."
   (interactive)
   (let ((filecontents (cdr (imenu--make-index-alist))))
     (switch-to-buffer (create-file-buffer "*Contents of file*"))
     (while filecontents
            (lfg-list-category (car filecontents))
            (setq filecontents (cdr filecontents)))))

(defun lfg-list-category (category)
   (insert (car category)) ; First element is name of category
   (insert ":\n")
   (setq category (reverse (cdr category)))
   (while category 
          (insert "  ")
          (insert (caar category))
          (insert "\n")
          (setq category (cdr category)))
   (insert "\n\n"))

;;;;;;;;; Functions for proper formatting.

(defun lfg-format-expression (&optional justify)
    (interactive)
   "Format a rule, lexical item, or template with spacing to indicate
    the level of disjunction.  Leave the carriage returns as they
    are.  Make sure the auto-fill-function is off."
   (save-excursion
     (let (beg-exp
           end-exp
           (config-pos-and-type (lfg-find-backwards-config-type)))
          (setq beg-exp (lfg-find-exp-beg config-pos-and-type))
          (setq end-exp (lfg-find-exp-end config-pos-and-type))
          ; Expression is more than 100 lines long.  Perhaps something
          ; has gone wrong.  Require user to call other function that
          ; does not check size of expression.
          (if (> (count-lines beg-exp (car end-exp)) 100)
            (error "Expression too large: > 100 lines.  Use M-x lfg-format-large-expression."))
          (save-restriction 
            (narrow-to-region beg-exp (car end-exp))
            (setq auto-fill-function nil)
            (goto-char (point-min))
            (lfg-format-single-expression (cdr config-pos-and-type) (cdr end-exp) justify) 
            ; return t
            t ))))

(defun lfg-format-large-expression (&optional justify)
    (interactive)
   "Same as lfg-format-expression, but does not check to see how big
    the expression is before trying to format it.  Format a rule,
    lexical item, or template with spacing to indicate 
    the level of disjunction.  Leave the carriage returns as they
    are.  Make sure the auto-fill-function is off."
   (save-excursion
     (let (beg-exp
           end-exp
           (config-pos-and-type (lfg-find-backwards-config-type)))
          (setq beg-exp (lfg-find-exp-beg config-pos-and-type))
          (setq end-exp (lfg-find-exp-end config-pos-and-type))
          (save-restriction 
            (narrow-to-region beg-exp (car end-exp))
            (setq auto-fill-function nil)
            (goto-char (point-min))
            (lfg-format-single-expression (cdr config-pos-and-type) (cdr end-exp) justify) 
            ; return t
            t ))))

(defun lfg-find-backwards-config-type ()
   "Find the type of the current block by looking in the previous
    portion of the buffer.  Return the cons of the point where the name
    of the block ends and what type of block it is."
   (save-excursion
    (let (config-name-end)
    (re-search-backward
     "\\(RULES\\|TEMPLATES\\|LEXICON\\|CONFIG\\|MORPHTABLE\\)[ \n\t]+([0-9]+.[0-9]+)" nil t)
    ; Block types that are in comments don't count.
    (while (lfg-in-comment (point) (point-min))
           (re-search-backward
        "\\(RULES\\|TEMPLATES\\|LEXICON\\|CONFIG\\|MORPHTABLE\\)[ \n\t]+([0-9]+.[0-9]+)" nil t))
    (setq config-name-end (match-end 0))
    (cond ((looking-at "RULES")
           (cons config-name-end "RULES"))
          ((looking-at "TEMPLATES")
           (cons config-name-end "TEMPLATES"))
          ((looking-at "LEXICON")
           (cons config-name-end "LEXICON"))
          ((looking-at "CONFIG")
           (cons config-name-end "CONFIG"))
          ((looking-at "MORPHTABLE")
           (cons config-name-end "MORPHTABLE"))
          (t
           (cons (point-min) "UNKNOWN"))))))

(defun lfg-find-exp-beg (config-pos-and-type)
   "Find the beginning of the current expression.
    If we are in a morphtable, find the beginning of the Lisp
    expression we are in.
    Otherwise find a previous occurrence of a period that is not in
    a comment and that comes after the beginning of the block.
    Otherwise go to the beginning of the block.
    Leave point at the beginning of the expression."
   (skip-chars-forward " \n\t")
   (let ((current-point (point))
         (config-pos (car config-pos-and-type)))
    (cond ((string= (cdr config-pos-and-type) "MORPHTABLE")
           (goto-char config-pos)
           (while (< (point) current-point)
                  (forward-sexp))
           (backward-sexp))
          ((re-search-backward "\\.[ \t\n]+" config-pos t)
           (goto-char (match-end 0))
           (if (lfg-in-comment (point) config-pos)
               (progn (goto-char (match-beginning 0))
                      (lfg-find-exp-beg config-pos-and-type))
               (progn (lfg-skip-comments)
                      (beginning-of-line))))
          (t 
           (goto-char config-pos)
           (skip-chars-forward " \t\n")
           (lfg-skip-comments)
           (beginning-of-line)))
           (point)))

; (defun lfg-in-comment (bufferpos config-pos)
;   "Adapted from how-many in replace.el -- alternatives seems to be too
;    slow.  Figure out whether bufferpos is in a comment or not by starting
;    at config-pos and then counting comment beginning and ending
;    characters (double quotes) to see if we are in or out of a comment."
;   (save-match-data
;    (let ((count 0))
;      (save-excursion
;      (goto-char config-pos)
;      (if (looking-at "\"")
;          (setq count (1+ count)))
;      (while (re-search-forward "[^`]\"" bufferpos t)
;          (forward-char -1)
;          (setq count (1+ count)))
;      (if (= 1 (% count 2))
;          t)))))

(defun lfg-in-comment (bufferpos config-pos)
  "Use sexp parser to determine whether we are in or out of a comment."
  (save-match-data
     (save-excursion
     (goto-char config-pos)
     (nth 3 (parse-partial-sexp config-pos bufferpos)))))

(defun lfg-find-exp-end (config-pos-and-type)
   "Find the end of the current expression.  Point is already at its
   beginning.  Return the cons of the end position and whether or not
   we are in a comment."
   (let (in-comment)
   (cond (; looking at a comment -- go to end
          (looking-at "\"")
          (forward-char 1)
          (skip-chars-forward "^\"")
          (forward-char 1)
          (setq in-comment t))
         (; looking at a block divider -- stay here
          (looking-at "----"))
         (; looking at a config name -- stay here
          (looking-at "[ \t]*[A-Z0-9]+[ \n\t]+[A-Z0-9]+[ \n\t]+\\(RULES\\|TEMPLATES\\|LEXICON\\|CONFIG\\|MORPHTABLE\\)[ \n\t]+([0-9]+.[0-9]+)"))
         (; in a morphtable -- treat as lisp
          (string= (cdr config-pos-and-type) "MORPHTABLE")
          (forward-sexp))
         (; in a lfg expression -- go to end
          (lfg-goto-end-def (point-max)))
         (; if all else fails go to the end of the file
          (goto-char (point-max))))
     (cons (point) in-comment)))

(defun lfg-format-single-expression (exptype in-comment justify)
   "The buffer has been narrowed to include just the expression to be
   formatted.  Format the buffer as an expression of type exptype.
   in-comment is true if we are in a comment between entries, not
   currently allowed.  We will ignore this case for now.
   We will only reformat if the expression is a rule, rule macro,
   template, or lexical item.  Future work: if exptype is UNKNOWN, can
   we figure out what to do?"
   (message "Formatting ...")
   (cond ((string= exptype "TEMPLATES")
          (lfg-format-template))
         ((string= exptype "LEXICON")
          (lfg-format-lexitem))
         ((string= exptype "RULES")
          (lfg-format-rule)))
   (message ""))

(defun lfg-format-template ()
   "Format a template.  Templates consist of a template name and
    possibly some arguments, an equals sign, and a set of constraints."
   (lfg-cleanup-expression)
   (indent-line-to 3)
   (let (indent-depth)
    ; Skip to the "="
    (skip-chars-forward "^=")
    ; Insert a space before the "=" if there isn't one already
    (forward-char -1)
    (or (looking-at " ")
        (progn (forward-char 1)
               (insert " ")))
    (forward-char 2)
    (skip-chars-forward " \t")
    (setq indent-depth (min 10 (current-column))) 
    (lfg-format-constraints (point-marker) (point-max-marker) indent-depth)))

(defun lfg-format-lexitem ()
   "Format a lexical entry.  Lexical entries consist of one or more
   subentries.  Subentries are separated by semicolons.  Each
   subentry is associated with  a set of constraints."
   (lfg-cleanup-expression) 
   (let (sublex-begin
         lex-fill-col
         constraints-fill-col)
   (goto-char (point-min))
   ; Skip over the lexical item 
   (lfg-skip-lexitem) 
   (setq lex-fill-col (max 10 (current-column)))
   (indent-to lex-fill-col) 
   ; Skip over the category and morphcode
   (lfg-skip-catmorphcode)
   (setq sublex-begin (point-marker))
   (setq constraints-fill-col (current-column))
   (while (re-search-forward ";" (point-max) t)
      (or (lfg-in-comment (point) (point-min))
          (progn (lfg-format-constraints sublex-begin (point-marker) constraints-fill-col)
                 ; Skip any comment on the same line
                 (skip-chars-forward " \t")
                 (lfg-skip-comments)
                 ; Skip to the next set of constraints.  Don't skip to
                 ;  the next line.
                 (skip-chars-forward " \t")
                 ; If we can go to the next line, then indent.
                 ; Otherwise stay here. 
                 (if (< 0 (skip-chars-forward " \n\t"))
                     (indent-line-to lex-fill-col))
                 (lfg-skip-catmorphcode)
                 (setq sublex-begin (point-marker))
                 (setq constraints-fill-col (current-column)))))
   (lfg-format-constraints sublex-begin (point-max-marker) constraints-fill-col)))

(defun lfg-skip-lexitem ()
   "Skip forward one lexitem.  Leave point at the beginning of the
   next word.  Take into account escape chars like ` or %."    
   (skip-chars-forward " \n\t" (point-max))
   (skip-chars-forward "^ \n\t" (point-max))
   (forward-char -1)
   (while (looking-at "\`")
        (forward-char 2)
        (skip-chars-forward "^ \n\t" (point-max))
        (forward-char -1))           
   (forward-char 1)
   (skip-chars-forward " \n\t" (point-max)))

(defun lfg-skip-catmorphcode ()
   (skip-chars-forward " \n\t" (point-max))
   "Skip forward over the category name."
   (skip-chars-forward "^ \n\t" (point-max))
   (skip-chars-forward " \n\t" (point-max))
   "Skip over the morphcode.  Stop before the semicolon." 
   (skip-chars-forward "^ \n\t;" (point-max))
   (skip-chars-forward " \n\t" (point-max)))

(defun lfg-format-rule ()
   "Rules or rule macros consist of a disjunction of lists of
   categories.   A category can be optional and appear in
   parentheses. Categories are separated by semicolons."
   (lfg-cleanup-expression)
   (indent-line-to 3)
   (goto-char (point-min))
   ; Skip to the operator: --> or =
   (re-search-forward "\\(-->\\|=\\)[ \t]*")
   (lfg-format-rule-block (point-max-marker) (min 10 (current-column)) 'cats))

(defun lfg-format-rule-block (end-marker fill-col block-type)
   "Format a unit on the rhs: a comment, a disjunction, an optional
   block, or a category with constraints."
    (skip-chars-forward " \n\t\*\+\#0-9")
    (let (comment-begin
          (block-end-marker (make-marker)))
    (cond (; Stop when we are at the end
           (>= (point) (marker-position end-marker)) t)
          (; Indent comments and skip over them
           (looking-at "\"")
           (skip-chars-backward " \t")
           (setq comment-begin (point))
           (forward-sexp 1)
           (indent-region comment-begin (point) fill-col)
           (lfg-format-rule-block end-marker fill-col block-type))
          (; We are in a disjunction 
           (looking-at "\{") 
           ; Insert a space after the open delimiter if there isn't one already
           (forward-char 1)
           (if (looking-at " ")
               (forward-char 1)
               (insert " "))
           (forward-char -2)
           (indent-to fill-col)
           ; Find the close delimiter
           (set-marker block-end-marker (scan-sexps (point) 1))
           (forward-char 2)
           (lfg-format-rule-block block-end-marker (+ 2 fill-col) 'disjunction)
           (lfg-format-rule-block end-marker fill-col block-type))
          (; We are in a regular expression
           (looking-at "\\[")
           (indent-to fill-col)
           ; Find the close delimiter
           (set-marker block-end-marker (scan-sexps (point) 1))
           (forward-char 1)
           (lfg-format-rule-block block-end-marker (+ 1 fill-col) 'cats)
           (lfg-format-rule-block end-marker fill-col block-type))
          (; We are in an optional category sequence
           (looking-at "\(")
           (indent-to fill-col)
           ; Find the close delimiter
           (set-marker block-end-marker (scan-sexps (point) 1))
           (forward-char 1)
           (lfg-format-rule-block block-end-marker (+ 1 fill-col) 'cats)
           (lfg-format-rule-block end-marker fill-col block-type))
          (; We are at an ignore category
           (looking-at "/")
           (indent-to fill-col)
           ; Insert a space if there isn't one already
           (forward-char 1)
           (if (looking-at " ")
               (forward-char 1)
               (insert " "))
           (lfg-format-rule-block end-marker (+ 2 fill-col) block-type))
          (; We are at a disjunction delimiter + space
           (looking-at "\| ")
           (indent-to (- fill-col 2))
           (forward-char 2)
           (lfg-format-rule-block end-marker fill-col block-type))
          (; We are at a disjunction delimiter |
           (looking-at "\|")
           (indent-to (- fill-col 1))
           (forward-char 1)
           (lfg-format-rule-block end-marker fill-col block-type))
          (; We are at a disjunction delimiter }
           (looking-at "\}")
           (indent-to (- fill-col 2))
           (forward-char 2)
           (lfg-format-rule-block end-marker fill-col block-type))
          (; We are at a negation or complementation marker
           (looking-at "\\(\\\\\\|\~\\)")
           (indent-to fill-col)
           (forward-char 1)
           (lfg-format-rule-block end-marker (+ 1 fill-col) block-type))
          (; We are at a macro with arguments.
           (looking-at "\@(")
           (indent-to fill-col)
           ; Go forward a char and find the end of the arg list
           (forward-char 1)
           (set-marker block-end-marker (scan-sexps (point) 1))
           ; Go to the end of the macro name
           (forward-char 1)
           (forward-word 1)
           (forward-char 1)
           (lfg-format-rule-block block-end-marker (current-column) 'cats)
           (lfg-format-rule-block end-marker fill-col block-type))
          ; We are at a category, possibly with constraints, or a
	  ; macro with no arguments.  Format
          ; the category and then the remainder of the block.
          (t 
           (indent-to fill-col)
           (lfg-format-rule-category end-marker block-type)
           (lfg-format-rule-block end-marker fill-col block-type)))))

(defun lfg-format-rule-category (end-marker block-type) 
   "Format the individual category/constraint beginning at
   point.  Leave point at the end of the category.
   Don't go past end-marker."
   ; Skip past the category label and any regexp notation
   (skip-chars-forward "^\:\n ")
   (let (end-category)
    (if (looking-at " *\:")
        ; There are some constraints associated with the category:
        (progn (setq end-category (lfg-find-category-end end-marker block-type))
               (skip-chars-forward " \:")
               (lfg-format-constraints (point-marker) end-category (current-column)))
        ; If the category has no constraints, go to the end of the line.
        (end-of-line))))

(defun lfg-find-category-end (end-marker block-type)
   "Find the end of the set of constraints associated with the current
   category."
   (save-excursion
   (let (close-delimiter)
   (cond ((equal block-type 'cats)
          (or (lfg-find-char-not-in-comment ";" (marker-position end-marker))
              end-marker))
         ((equal block-type 'disjunction)
          (if (lfg-find-char-not-in-comment "\\(;\\|\|\\|\{\\|\}\\)" (marker-position end-marker))
              (progn (forward-char -1)
                     (cond ((looking-at ";") 
                            (forward-char 1)
                            (point-marker))
                           ((or (looking-at "\|")
                                (looking-at "\}"))
                            (skip-chars-backward "\|\} \n\t")
                            (point-marker))
                           ((looking-at "\{")
                            (goto-char (scan-sexps (point) 1))
                            (lfg-find-category-end end-marker block-type))))
              end-marker))))))

(defun lfg-find-char-not-in-comment (char search-end)
   "Find an occurrence of char that is not in a comment and that
   occurs before search-end.  Leave point after char and return
   point-marker.  Fail if no such char is found."
   (if (re-search-forward char search-end t)
       (if (lfg-in-comment (point) (point-min))
           (lfg-find-char-not-in-comment char search-end)
           (point-marker))))

(defun lfg-cleanup-expression ()
   "Record where the expression begins, remove all extra tabs and
   spaces, and indent the first line of the expression to the
   beginning indentation."
   (let (begin-col)
    (goto-char (point-min))
    (skip-chars-forward " \n\t")
    (setq begin-col (current-column))
    ;(goto-char (point-min))
    ;(if (re-search-forward "[ \n\t]*\\.\\([ \n\t]*\\)\\'" nil t)
        ;(replace-match "\\.\\1" nil nil))
    (goto-char (point-min))    
    (while (re-search-forward "[ \t]+" nil t)
           (replace-match " " nil nil))
    (goto-char (point-min))
    (indent-line-to begin-col)))

(defun lfg-format-constraints (begin-marker end-marker fill-col)
   "Format a set of constraints.  Sets of constraints are template
   definitions, constraints on rule nodes, or constraints on lexical
   items.  Don't touch the first line.  Leave point at the end of the
   set of constraints." 
   (goto-char (marker-position begin-marker))
   (let ((next-fill-col (lfg-next-fill-col fill-col)))
     (forward-line 1)
     (lfg-indent-constraint-region end-marker next-fill-col)
     (goto-char (marker-position end-marker))))

(defun lfg-indent-constraint-region (end-marker fill-col)
   "Indent each line of a set of constraints according to levels of
   disjunction."
   (let ((current-fill-col fill-col))
    (while (< (point) (marker-position end-marker))
           (lfg-indent-line-to current-fill-col)
           (setq current-fill-col (lfg-next-fill-col current-fill-col))
           (forward-line 1))))

(defun lfg-next-fill-col (fill-col)
   "On the basis of the open and close delimiters in the current line,
   figure out what the indentation should be for the next line."
   (skip-chars-forward " \t")
   (let (start-point end-point)
   (cond ((looking-at "{ ") 
          (forward-char 2)
          (lfg-next-fill-col (+ 2 fill-col)))
         ((looking-at "{") 
          (forward-char 1)
          (insert " ")
          (lfg-next-fill-col (+ 2 fill-col)))
         ((looking-at "~\\[") 
          (forward-char 2)
          (lfg-next-fill-col (+ 2 fill-col)))
         ((looking-at "\\[") 
          (forward-char 1)
          (insert " ")
          (lfg-next-fill-col (+ 2 fill-col)))
         (t (setq start-point (point))
            (end-of-line)
            (setq end-point (point))
            (goto-char start-point)
            (- fill-col 
                  (* 2 (- (lfg-how-many-region "\\(\}\\|\\]\\)" start-point end-point)
                       (lfg-how-many-region "\\(\{\\|\\[\\)" start-point end-point))))))))

(defun lfg-indent-line-to (fill-col)
   "Indent the line to fill-col, unless it starts with a disjunction
   delimiter | or }.  In that case indent it out some."
   (skip-chars-forward " \t")
   (cond ((looking-at "| ") 
          (indent-line-to (- fill-col 2)))
         ((looking-at "|") 
          (indent-line-to (- fill-col 1)))
         ((looking-at "\}")
          (indent-line-to (- fill-col 2)))
         (t (indent-line-to fill-col))))

(defun lfg-how-many-region (regexp beg end)
  "Adapted from term.el, where it is called term-how-many-region. Return
  number of matches for REGEXP from BEG to END."
  (let ((count 0))
    (save-excursion
      (save-match-data
	(goto-char beg)
	(while (and (re-search-forward regexp end t)
                    (not (lfg-in-comment (point) (point-min))))
	  (setq count (1+ count)))))
    count))

;;;;;;;;; Display an expression without displaying comments.

(defun lfg-display-expression ()
    (interactive)
   "Display a rule, lexical item, or template in another read-only window,
    without displaying comments.  The displayed expression is just a
    copy of the expression in the main file."
   (let (beg-exp
         end-exp
         (lfg-expression-display-buffer 
                (generate-new-buffer "*LFG Expression Display*"))
         (config-pos-and-type (lfg-find-backwards-config-type)))
        (display-buffer lfg-expression-display-buffer)
        (save-excursion
          (setq beg-exp (lfg-find-exp-beg config-pos-and-type))
          (setq end-exp (lfg-find-exp-end config-pos-and-type))
          (copy-to-buffer lfg-expression-display-buffer beg-exp (car end-exp))
          (set-buffer lfg-expression-display-buffer)
          (lfg-mode)
          (lfg-delete-comments)
          (setq buffer-read-only t)))        
         ; return t
         t )

(defun lfg-display-file ()
    (interactive)
   "Display a file of LFG expressions (rules, lexical items,
    templates) in another read-only window, without displaying comments."
   (let ((lfg-file-display-buffer 
                (generate-new-buffer "*LFG File Display*"))
         (lfg-point (point)))
        (display-buffer lfg-file-display-buffer)
        (save-excursion
          (copy-to-buffer lfg-file-display-buffer (point-min)
			  (point-max))
          (set-buffer lfg-file-display-buffer)
          (goto-char lfg-point) 
          (point-to-register 'L)   
          (goto-char (point-min))     
          (lfg-delete-comments)
          (jump-to-register 'L)
          (setq buffer-read-only t)))
        ; return t
        t)

(defun lfg-delete-comments ()
    "Remove the comments from an LFG expression or file."
     ; Eat up lines which consist of a comment
    (while (re-search-forward "\n[ \t]*\"[^\"]+\"[ \t]*$" nil t)
                  (replace-match ""))     
    (goto-char (point-min))
     ; Eat up lines with { or | followed by space, then comment, newline
    (while (re-search-forward "\\([{|]\\)[ \t]+\"[^\"]+\"[ \t]*\n[ \t]*" nil t)
                  (replace-match "\\1 "))    
    (goto-char (point-min))
     ; Same as above, but with no space after { or |
    (while (re-search-forward "\\([{|]\\)\"[^\"]+\"[ \t]*\n[ \t]*" nil t)
                  (replace-match "\\1"))    
    (goto-char (point-min))
     ; Replace comments followed by real stuff with blanks
    (while (re-search-forward "\n\\([ \t]*\"[^\"]+\"[ \t]*\\)+\\([^\"]\\)"
			      nil t)
                  (let (lfg-current-column)
                       (goto-char (match-beginning 2))
                       (setq lfg-current-column (current-column))
                       (replace-match "" nil t nil 1)
                       (indent-line-to lfg-current-column)))
    (goto-char (point-min))
     ; Eat up any remaining comments without eating up lines
    (while (re-search-forward "\"[^\"]+\"" nil t)
                  (replace-match ""))
    (goto-char (point-min))
     ; Eat up blank space preceding expression-ending punctuation ;
     ; . ] ) }
    (while (re-search-forward "[ \t\n]+\\([].;)}]\\)" nil t)
                  (replace-match " \\1"))
    (goto-char (point-min))
     ; Finally, eat up excess blank lines, but keep the indentation
     ; before whatever follows the last newline
    (while (re-search-forward "\n[ \t]*\\(\n[ \t]*\\)+\\(\n[ \t]*\\)" nil t)
                  (replace-match "\n\\2")))
    

;;;;;;;;;  Comment a region

(defconst lfg-comment-char ?"
  "Character surrouding comments in XLE grammars. 
Used by the function lfg-comment-region.") 

(defun lfg-comment-region (begin end &optional arg)
  "Comment or uncomment region. 

In case begin and end of region are both inequal to \", region is
surrounded by \". Any \" inside region is replaced by \"\".

Otherwise, \"s around region are removed. All \" pairs inside region
are replaced by a single \". Note that region must contain both 
delimiting \"s!."
  (interactive "r\n")
  
  (save-excursion
    (save-restriction
      (let ((comment-p t)) ; flag saying we should comment region
	
	;; determine whether we must comment or uncomment
	(if (char-equal (char-after begin) lfg-comment-char)
	    (if (char-equal (char-after (- end 1)) lfg-comment-char)
		(setq comment-p nil) ; uncomment only if begin and end are at ?" chars
	      )
	  )
	
	(if comment-p
	    (progn ;; add comment

	      ;; insert " at begin and end
	      (goto-char begin)
	      (insert "\"") ; insert " after markers to leave mark before it
	      (goto-char (+ end 1)) ; offset to account for 1st insertion
	      (insert-before-markers "\"") ; insert " before markers to move point onward

	      ;; narrow to restrict replacement
	      (narrow-to-region (+ begin 1) (+ end 1)) ; account for insertion

	      ;; replace single " by double ""
	      (goto-char (+ begin 1))
	      (perform-replace "\"" "\"\"" nil nil nil)
	      )
	  (progn ;; remove comment

	    ;; remove " at begin and end (we're sure both places are "!)
	    (goto-char (- end 1))
	    (delete-char 1)
	    (goto-char begin)
	    (delete-char 1)

	    ;; narrow to restrict replacement
	    (narrow-to-region begin (- end 2)) ; account for deletions

	    ;; replace double "" by single "
	    (goto-char begin)
	    (perform-replace "\"\"" "\"" nil nil nil)
	    )
	  )
	)
      )
    )
  
  ;; ensure proper fontification
  (font-lock-fontify-buffer)
  )


;;;;;;;; Compare two runs of XLE.

(defun xle-run-compare ()
   "Compare results of two different XLE runs on the same (or similar)
    files.  Returns a buffer indicating which sentences got no parses
    on one but not the other run."
   (interactive)
   (let* ((buffer1 
           (find-file (read-file-name "File with first run results: ")))
          (buffer2 
           (find-file (read-file-name "File with second run results: ")))
          (list1 (xle-parse-buffer buffer1))
          (list2 (xle-parse-buffer buffer2)))
         (or (and (/= (length list1) (length list2))
                  (error "The files do not have the same number of sentences!"))
               (xle-compare-parses list1 list2))))

(defun xle-run-compare-full ()
   "Compare results of two different XLE runs on the same (or similar)
    files.  Returns a buffer with information about how the number of
    parses in the two runs differ."
   (interactive)
   (let* ((buffer1 
           (find-file (read-file-name "File with first run results: ")))
          (buffer2 
           (find-file (read-file-name "File with second run results: ")))
          (list1 (xle-parse-buffer buffer1))
          (list2 (xle-parse-buffer buffer2)))
         (or (and (/= (length list1) (length list2))
                  (error "The files do not have the same number of sentences!"))
               (xle-compare-parses-full list1 list2))))

(defun xle-parse-buffer (buffer)
   "Analyzes a buffer containing results of an XLE run.  Returns a
    list of pairs, where the first member is a string and the second
    member is the string representing the number of parses that string
    got on that run." 
   (set-buffer buffer)
   (goto-char (point-min))
   (let ((list '())
         (sentence-number 1))
        (while (re-search-forward "\\([^#][^\n]*\\)[ \t]+(\\([0-9]+[!][ \t]+\\)?\\([~]\\)?\\([*]\\)?\\([-0-9.E+]+\\|Inf\\)[ \t]+[0-9.E+]+[ \t]+[0-9.E+]+)\n\n" (point-max) t)
               (setq list (cons (cons (buffer-substring
					  (match-beginning 5)
					  (match-end 5))
                                       (cons (buffer-substring
				               (match-beginning 1)
					       (match-end 1))
                                             (int-to-string sentence-number)))
                                 list))
               (setq sentence-number (+ 1 sentence-number)))
        (or list
            (error "File does not contain performance information"))))

(defun xle-compare-parses (list1 list2)
   "Compare each member of list1 and list2.  The members should be
    parses of the same sentence (modulo possible markup).  If the
    (string representing the) number of parses is the same, don't do
    anything.  Otherwise report any sentence that had no parses on one
    or the other run." 
   (let ((xle-compare-buffer (set-buffer (generate-new-buffer "*XLE Run Comparison Results*")))
         zero1 zero2)
        (while list1
           (if (string-equal (caar list1) (caar list2))
               t
               (let ((parses1 (xle-parse-count (caar list1)))
                     (parses2 (xle-parse-count (caar list2)))
                     (parsereport 
                            (cons 
                                (length (cadr (car list2)))
                                (concat
                                    "# Sentence number "
                                    (cddr (car list2))
                                    "  (" 
                                    (caar list1)
                                    "->"
                                    (caar list2)
                                    "):\n\n"
                                    (cadr (car list2))))))
                    (cond
                       ((equal 0 parses1)
                        (setq zero1 (cons parsereport zero1)))
                       ((equal 0 parses2)
                        (setq zero2 (cons parsereport zero2))))))
           (setq list1 (cdr list1))
           (setq list2 (cdr list2)))
          (if zero1 
              (progn (insert "\n\n#***DIDN'T PARSE ON FIRST RUN, PARSED ON SECOND RUN***")
                     (xle-insert-strings zero1))
              (insert "\n\n#***ALL STRINGS THAT PARSED ON THE SECOND RUN ALSO PARSED ON THE FIRST RUN.***"))
          (if zero2 
              (progn (insert "\n\n#***PARSED ON FIRST RUN, DIDN'T PARSE ON SECOND RUN***")
                  (xle-insert-strings zero2))
              (insert "\n\n#***ALL STRINGS THAT PARSED ON THE FIRST RUN ALSO PARSED ON THE SECOND RUN.***"))
          (switch-to-buffer xle-compare-buffer)))

(defun xle-compare-parses-full (list1 list2)
   "Compare each member of list1 and list2.  The members should be
    parses of the same sentence (modulo possible markup).  If the
    (string representing the) number of parses is the same, don't do
    anything.  Otherwise categorize the sentence in the second list
    by comparison to the number of parses reported in the first list." 
   (let ((xle-compare-buffer (set-buffer (generate-new-buffer "*XLE Run Comparison Results*")))
         zero1 zero2 more1 more2)
        (while list1
           (if (string-equal (caar list1) (caar list2))
               t
               (let ((parses1 (xle-parse-count (caar list1)))
                     (parses2 (xle-parse-count (caar list2)))
                     (parsereport
                            (cons 
                                (length (cadr (car list2)))
                                (concat
                                    "# Sentence number "
                                    (cddr (car list2))
                                    "  (" 
                                    (caar list1)
                                    "->"
                                    (caar list2)
                                    "):\n\n"
                                    (cadr (car list2))))))
                    (cond
                       ((equal "Inf" parses1)
                        (if (equal 0 parses2)
                            (setq zero2 (cons parsereport zero2))
                            (setq more1 (cons parsereport more1))))
                       ((equal "Inf" parses2)
                        (if (equal 0 parses1)
                            (setq zero1 (cons parsereport zero1))
                            (setq more2 (cons parsereport more2))))
                       ((equal 0 parses1)
                        (setq zero1 (cons parsereport zero1)))
                       ((equal 0 parses2)
                        (setq zero2 (cons parsereport zero2)))
                       ((> parses1 parses2)
                        (setq more1 (cons parsereport more1)))
                       ((> parses2 parses1)
                        (setq more2 (cons parsereport more2))))))
           (setq list1 (cdr list1))
           (setq list2 (cdr list2)))
          (if zero1 
              (progn (insert "\n\n#***DIDN'T PARSE ON FIRST RUN, PARSED ON SECOND RUN***")
                     (xle-insert-strings zero1))
              (insert "\n\n#***ALL STRINGS THAT PARSED ON THE SECOND RUN ALSO PARSED ON THE FIRST RUN.***"))
          (if zero2 
              (progn (insert "\n\n#***PARSED ON FIRST RUN, DIDN'T PARSE ON SECOND RUN***")
                  (xle-insert-strings zero2))
              (insert "\n\n#***ALL STRINGS THAT PARSED ON THE FIRST RUN ALSO PARSED ON THE SECOND RUN.***"))
          (if more1 
              (progn (insert "\n\n#***MORE PARSES ON FIRST RUN THAN ON SECOND RUN:***")
                     (xle-insert-strings more1))
              (insert "\n\n#***NO SENTENCES THAT PARSED ON THE FIRST RUN RECEIVED MORE PARSES ON THE SECOND RUN.***"))
          (if more2 
              (progn (insert "\n\n#***MORE PARSES ON SECOND RUN THAN ON FIRST RUN***")
                     (xle-insert-strings more2))
              (insert "\n\n#***NO SENTENCES THAT PARSED ON THE FIRST RUN RECEIVED FEWER PARSES ON THE SECOND RUN.***"))
          (switch-to-buffer xle-compare-buffer)))

(defun xle-parse-count (parsecountstring)
  "Figure out how many parses a string had.  Inf seems to be a
   possible number."
   (let* ((tofirstplus (substring parsecountstring 0 (string-match "+" parsecountstring)))
          (num (string-to-number tofirstplus)))
   (cond ((string-equal parsecountstring "Inf")
          "Inf")
         ((string-equal tofirstplus parsecountstring)
          ; If this is true, there are no +'s in parsecountstring
          (if (< num 0) 
              ; It is a negative number, meaning there were zero parses
              0
              ; It is a positive integer representing the number of parses
              num))
         ((string-equal tofirstplus (substring tofirstplus 0 (string-match "E" tofirstplus)))
          ; If this is true, the part of the string in tofirstplus
          ; does not have an E and thus represents the optimals, which
          ; is what we want to count 
          num)
         ; Otherwise the first + in the string was part of an
         ; expression in scientific notation.  We now want to look at
         ; the part of the string after the first +.
         (t
          (string-to-number (substring parsecountstring 
                     0 
                     (+ (length tofirstplus)
                        (+ 1 (length (substring (substring parsecountstring 
                                                           (+ 1 (length tofirstplus)))
                                                0 
                                                (string-match "+" (substring parsecountstring (+ 1 (length tofirstplus))))))))))))))

(defun xle-insert-strings (strings)
   "Insert the appropriate strings into the current buffer."
    (let ((currentstrings (sort strings (lambda (a b) (< (car a)(car b))))))
         (while currentstrings
                (insert "\n\n")
                (insert (cdar currentstrings))
                (setq currentstrings (cdr currentstrings)))))



;;;;;;;;; Display comments from .lfg files

; Gather comments from files referenced in configuration file and
; display them in two buffers: one for all of the comments, and one
; for the examples 

(defun lfg-display-comments (&optional which-files)
  "Find the comments in the relevant files and submit them for display
   in a new comments buffer and, if present, a buffer of examples.
   Optional arguments are the atoms all-files, which lists comments in all
   files with the extension .lfg in the current directory; or
   grammar-files, which asks for a config file and lists all comments in
   the files mentioned in the config.  The default is to list comments
   only in the current buffer."
  (interactive)
  (let (lfg-comment-files
        lfg-full-comment-output
        config-file
        config-dir)
     (cond 
       ((eq which-files 'all-files)
          (and (message "Displaying comments from .lfg files in %s" default-directory)
               (setq lfg-comment-files (directory-files default-directory nil ".*.lfg$"))))
       ((eq which-files 'grammar-files)
          (and (message "Displaying comments from files mentioned in config file %s"
                   (setq config-file (read-file-name "File with config information: ")))
                   (setq config-dir (file-name-directory config-file))
                   (setq lfg-comment-files (cons config-file
						 (lfg-get-grammar-files-from-config config-file config-dir)))))
      (t
        (and (message "Displaying comments from %s" (buffer-file-name))
             (setq lfg-comment-files (list (buffer-file-name))))))
     (while lfg-comment-files
         (setq lfg-full-comment-output 
               (append (lfg-gather-comments-file (car lfg-comment-files))
		     lfg-full-comment-output))
         (setq lfg-comment-files (cdr lfg-comment-files)))
     (if lfg-full-comment-output 
         (lfg-write-comments-and-examples (lfg-comment-clean-output
					   lfg-full-comment-output))
         (message "No comments found"))))

(defun lfg-display-comments-all ()
   "Displays comments for all .lfg files in current directory."
    (interactive)
    (lfg-display-comments 'all-files))

(defun lfg-display-comments-config ()
   "Prompts for config file; displays comments for all files mentioned
    in config file."
    (interactive)
    (lfg-display-comments 'grammar-files))

(defun lfg-get-grammar-files-from-config (file dir)
  "Return a list of files mentioned in the config data in file."
   (save-excursion 
   (set-buffer (find-file file))
   (goto-char (point-min))
   (let (grammar-files)
      (if (search-forward "CONFIG" nil t)
          (if (search-forward "FILES" nil t)
              (progn (skip-chars-forward " \n\t")
                     (while (not (looking-at
				  "ROOTCAT\\|LEXENTRIES\\|RULES\\|FEATURES[ \t]\\|TEMPLATES\\|MORPHOLOGY\\|GOVERNABLERELATIONS\\|SEMANTICFUNCTIONS\\|NONDISTRIBUTIVES\\|EPSILON\\|EXTERNALATTRIBUTES\\|OPTIMALITYORDER\\|GENOPTIMALITYORDER\\|PARAMETERS"))
                           ; We are looking at a filename
                           (if (looking-at "\\(.+\\.lfg\\)\\.?[ \t]*$")
                               (setq grammar-files (cons (concat dir
							   (buffer-substring (point) (match-end 1)))
                                                      grammar-files)))
                           (end-of-line)
                           (skip-chars-forward " \n\t")))
              (error "No file names found in config section"))                          
          (error "No config information found in file " file))
          grammar-files)))
  
(defun lfg-comment-clean-output (comment-output)
   "Take the raw comment output, replace abbreviations with full
   labels, and reverse the list to match the input order." 
   (let ((list-element 0)
         comment-element)
        (while (setq comment-element (nth list-element comment-output))
           (setq list-element (+ 1 list-element))
           (lfg-replace-comment-abbrev comment-element)))
   (reverse comment-output))

(defun lfg-replace-comment-abbrev (comment-element)
   "If the first element of the list is an abbreviation, replace it."
   (cond 
       ((string-equal (car comment-element) "FEAT")
        (setcar comment-element "FEATURE"))
       ((string-equal (car comment-element) "TEMP")
        (setcar comment-element "TEMPLATE"))
       ((string-equal (car comment-element) "CAT")
        (setcar comment-element "CATEGORY"))
       ((string-equal (car comment-element) "LEX")
        (setcar comment-element "LEX-ENTRY"))
       ((string-equal (car comment-element) "OT")
        (setcar comment-element "OT-MARK"))
       ((string-equal (car comment-element) "PARAM")
        (setcar comment-element "PARAMETER"))
       ((string-equal (car comment-element) "EX")
        (setcar comment-element "EXAMPLES"))
       (t)))

(defun lfg-write-comments-and-examples (lfg-comments)
   "Clean up and sort the comments which have been gathered up and write them to
   the file *LFG Comments*.  Also, if there are any examples, write
   them out in a separate example buffer."
   (let ((time (current-time-string)))
     (lfg-write-comments lfg-comments time)
     (lfg-write-examples lfg-comments time)))
       
(defun lfg-write-comments (lfg-comments time)
   "Write a list of lists of comments to the buffer *LFG Comments*."
   (switch-to-buffer (create-file-buffer "*LFG Comments*"))
   (insert "# Comments collected " time "\n\n")
   (make-local-variable 'tab-stop-list)
   (setq tab-stop-list '(2 12 30))
   (let ((comment-type (caar lfg-comments)))
      (while lfg-comments
            (insert "\n")
            (if comment-type
                (insert comment-type)
                (insert "**NIL**"))
            (insert "\n\n")
            (lfg-write-comment-body (cdar lfg-comments))
            (setq lfg-comments (cdr lfg-comments))
            (while (string= comment-type (caar lfg-comments))
                   (lfg-write-comment-body (cdar lfg-comments))
                   (setq lfg-comments (cdr lfg-comments)))
            (if (car lfg-comments)
                (setq comment-type (caar lfg-comments)))))
   (goto-char (point-min)))

(defun lfg-write-comment-body (lfg-single-comment)
   "Write a single comment about a type to the current buffer."
   (tab-to-tab-stop)	
   (if (nth 0 lfg-single-comment)
       (insert (nth 0 lfg-single-comment))
       (insert "**NIL**"))
   (tab-to-tab-stop)	
   (insert "(in ")
   (if (nth 1 lfg-single-comment)
        (insert (nth 1 lfg-single-comment))
        (insert "**NIL**"))
   (insert ")")
   (tab-to-tab-stop)
   (if (nth 2 lfg-single-comment)
       (insert (nth 2 lfg-single-comment))
       (insert "**NIL**"))
   (insert "\n\n"))

(defun lfg-write-examples (lfg-comments time)
   "Write the examples from the comments to the example buffer if we find
    any."
   (while (and lfg-comments 
               (not (string= "EXAMPLES" (caar lfg-comments))))
          (setq lfg-comments (cdr lfg-comments)))
   (if lfg-comments
       (progn (split-window)
              (other-window 1)
              (switch-to-buffer (create-file-buffer "*LFG Examples from comments*"))
              (insert "# Examples collected " time "\n\n")
              (make-local-variable 'tab-stop-list)
              (setq tab-stop-list '(5 10 12))
              (while lfg-comments 
                     (if (string= "EXAMPLES" (caar lfg-comments))
                         (let ((comment-name (nth 1 (car lfg-comments)))
                               (def-name (nth 2 (car lfg-comments))))
                              (insert "\n\# ")
                              (if comment-name
                                  (insert comment-name)
                                  (insert "**NIL**"))
                              (insert " ")
                              (if def-name
                                  (insert def-name)
                                  (insert "**NIL**"))
                              (insert "\n")
                              (while (and (string= "EXAMPLES" (caar lfg-comments))
                                          (string= comment-name (nth 1 (car lfg-comments)))
                                          (string= def-name (nth 2 (car lfg-comments))))
                                    (insert "\n")
                                    (tab-to-tab-stop)
                                    (if (nth 3 (car lfg-comments))
                                        (insert (nth 3 (car lfg-comments)))
                                        (insert "**NIL**"))
                                    (insert "\n")
                                    (setq lfg-comments (cdr lfg-comments))))
                         (setq lfg-comments (cdr lfg-comments))))
               (goto-char (point-min)))))

(defun lfg-gather-comments-file (grammarfile)
  "Return all the comments in grammarfile."
    (set-buffer (find-file grammarfile))
    (let ((current-point (point))
          beg-def
          beg-block
          end-block
          comment-begin
          def-name
          comments-output)
    (goto-char (point-min))
    (while (re-search-forward "\\(RULES\\|TEMPLATES\\|LEXICON\\)[ \n\t]+([0-9]+.[0-9]+)" nil t)
           (setq beg-block (match-end 0))
           (cond ((re-search-forward "^[ ]*----[^-]" nil t)
                  (setq end-block (match-beginning 0)))
                 (t
                  (setq end-block (point-max))))
           (save-restriction 
            (narrow-to-region beg-block end-block)
            (goto-char (setq beg-def beg-block))
            (while (and (re-search-forward
			 "\\(COMMENT\\|COM\\|COM-FILE\\|COMMENT-FILE\\)[{]" nil t)
                        (lfg-in-comment (point) beg-def))
               (setq comment-begin (- (point) 1))
               ; If it is a regular comment,  get the name of the
	       ; definition that point is in; if it is a file comment,
	       ; get the name of the file.
               (cond 
                 ((or (string-equal (buffer-substring-no-properties (match-beginning 0)
                                                      (match-end 0))
                                    "COM-FILE{")
                      (string-equal (buffer-substring-no-properties (match-beginning 0)
                                                      (match-end 0))
                                    "COMMENT-FILE{"))
                   (setq def-name (buffer-name)))
                 (t (lfg-goto-beg-def beg-def)
                    (setq beg-def (point))
                    (setq def-name (lfg-forward-item (point-max)))))
               ; Now assemble the rest of the comment.
               (goto-char comment-begin)
               (setq comments-output 
                     (append (lfg-assemble-comment def-name)
			     comments-output)))
               (message (caar comments-output))
               (goto-char (point-max))))
        ; Restore point to original position, but leave everything else
        ; alone (this does not do all the work of save-excursion -- just
        ; restores point).  This allows point to appear in appropriate position
        ; when an error (e.g. missing close bracket) is found; with
        ; save-excursion, everything is restored and it is impossible to
        ; tell where the error is.
        (goto-char current-point)
        comments-output))

(defun lfg-goto-beg-def (beg-def)
   "Find the BEGINNING of the definition that point is currently in.
    We are currently in a comment.  Go back to a previous period that
    is not in a comment (this will be the end of the previous
    definition), or to the beginning of the file if there is 
    no previous period."
   (while (lfg-in-comment (point) beg-def)
          (or (re-search-backward "[.]" nil t)
              (goto-char beg-def)))
   (if (and (not (lfg-in-comment (point) beg-def))
            (looking-at "[.]"))
       (forward-char 1)))

(defun lfg-assemble-comment (def-name)
   "We are looking at a comment that needs to be saved.  We have
    determined def-name, the name of the definition we are in.  Form a
    list of lists, each consisting of the first word (indicating the type
    of comment), def-name, the name of the comment, and the text of the
    comment.  Leave point at end of expression being searched."
   (save-excursion
    (let ((beg-comment (+ 1 (point)))
          (end-comment (- (save-excursion (forward-sexp)(point)) 1))
         comment-type
         comment-name
         comment-text
         comment)
        (goto-char beg-comment)
        ; First get the comment text
        (if (looking-at "\{")
            ; We are in a multi-comment that needs to be expanded
            (while (looking-at "\{")
                   (forward-sexp)
                   (skip-chars-forward " \n\t"))
            ; Otherwise we just go forward two words
            (and (forward-word 2)
                 (skip-chars-forward " \n\t")))
        (setq comment-text (buffer-substring-no-properties (point)
							  end-comment))
        ; Now we need the comment type and comment name.  There might
	; be several pairs of these.
	(goto-char beg-comment)
        (if (looking-at "\{")
            ; We are in a multi-comment that needs to be expanded
            (while (looking-at "\{")
                   (forward-char 1)
                   (setq comment-type (lfg-forward-item (point-max)))
                   (forward-word 1)
                   (setq comment-name (lfg-forward-item (point-max)))
                   (forward-word 1)
                   (skip-chars-forward " \n\t\}")
                   (setq comment (cons (list comment-type
					     comment-name 
					     def-name 
                                             comment-text)
				       comment)))
            (progn (setq comment-type (lfg-forward-item (point-max)))
                   (forward-word 1)
                   (setq comment-name (lfg-forward-item (point-max)))
                   (setq comment (list (list comment-type
			                     comment-name 
			                     def-name 
                                             comment-text)))))
         comment)))

;;; imenu-go.el begins here
;;; imenu-go.el - Go to definition of item using imenu and tags

;; Copyright (C) 1995  Ilya Zakharevich

;; Author: Ilya Zakharevich <ilya@math.mps.ohio-state.edu>

;; This file is not a part of GNU Emacs, but is made available under
;; the same conditions.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary ============================================================

;;; To use this package add 

;;; (autoload 'imenu-go-find-at-position "imenu-go"
;;;  "Go to the definition of the current word." t) 
;;; (autoload 'imenu-go--back "imenu-go"
;;;  "Return back to a position saved during `imenu-go-find-at-position'." t) 

;;; and bindings like

;;; (global-set-key [M-S-mouse-2] 'imenu-go-find-at-position)
;;; (global-set-key "\e\"" 'imenu-go-find-at-position)
;;; (global-set-key [M-S-C-mouse-2] 'imenu-go--back)
;;; ;(global-set-key "\e'" 'imenu-go--back)  ; Conflicts with defined key.
;;; (global-set-key [?\C-\"] 'imenu-go--back)

;;; to your .emacs file. The usability if this package decreases a lot
;;; unless you have a simple access to `imenu', like in

;;; (global-set-key [M-S-down-mouse-3] 'imenu)

;;; To cache information about interesting places you should either
;;; run `imenu' in the interesting buffers, or run `etags *.c *.h' (or
;;; whatever) on interesting files. After this calling
;;; `imenu-go-find-at-position' when the cursor or pointer is over the
;;; interesting word will warp you to the definition of this word. You
;;; can unwind this warping by doing `imenu-go--back'.

;;; Alternately, for Emacs-Lisp hacking you may install package
;;; `find-function', which will be automatically used in Emacs Lisp mode:
;;;      (autoload 'find-function "find-func" nil t)


;;; Part of functionality of this package belongs to imenu.el, but is
;;; not there.

;;;; Changes:
;;; Now `pop-to-window' with `pop-up-windows' set to nil instead of 
;;; `switch-to-window'.
;;; Search is made case-sensitive. Minor bugs corrected.
;;;; 08/97:
;;; Bug in `let': "will not restore local variables if buffer is changed" 
;;;  circumvented.
;;; Will use `find-function' if present/autoloadable.
;;;; 09/97:
;;; Will use `find-function' in `debugger-mode' and `lisp-interaction-mode' as well.
;;; Workaround for a reported bug in `imenu--generic-function'.

(require 'imenu)

(or (fboundp 'point-to-mouse-maybe)	; Is defsubst fboundp-ing?
(defsubst point-to-mouse-maybe ()
  "Moves point to the position of the click if the last event is click.
Intended for use in commands that can be bound both to mouse and keyboard."
  (and (listp last-input-event) (mouse-set-point last-input-event)))
    )

(defvar imenu---can-seen)		; Keeps seen alists to avoid recursion

(defun imenu---can-find (str)
  "Check whether the string STR is known to `imenu'."
  (let (imenu---can-seen)
    (and (boundp 'imenu--index-alist) imenu--index-alist
	 (imenu---in-alist str imenu--index-alist))))

(defun imenu---can-find-all-buffers (str)
  "Check whether the string STR is known to `imenu' in some buffers."
  (let ((blist (buffer-list)) buffer imenu---can-seen done)
    (save-excursion
      (while (and (not done) blist)
	(setq buffer (car blist) blist (cdr blist))
	(set-buffer buffer)
	(setq done (imenu---can-find str))))
    (if done (list buffer done))))

(defun imenu---in-alist (str alist)
  "Check whether the string STR is contained in multi-level ALIST."
  (let (elt head tail res prob-res (initial alist))
    (while alist
      (setq elt (car alist) alist (cdr alist) head (car elt) tail (cdr elt))
      (if head
	  (if (string= str head) (setq alist nil res tail)
	    (and (listp tail)
		 ;; Avoid recursion
		 (setq prob-res (or (if (memq tail imenu---can-seen) nil
				      (setq imenu---can-seen 
					    (cons tail imenu---can-seen))
				      (imenu---in-alist str tail))
				    prob-res)))
	    (or prob-res 
		(if (string-match (concat "\\<" (regexp-quote str) "\\>") head)
		    (setq prob-res tail))))))
      (or res prob-res)))

(defvar imenu---stack nil "List of positions to return back to later.")

;;;###autoload
(defun imenu-go-find-at-position (&optional default)
  "Go to a definition of the word under cursor or pointer.
Tries to find `imenu'-information on the current word, if cannot,
tries to find it as an Emacs function (in Emacs Lisp mode, and if
`find-function' is available), if cannot, falls back to tags search.
Saves the position to allow return back by `imenu-go--back'.  Can be
bound both to mouse and keyboard event.  Will show found buffer in a
different window unless the current window is the only one in the
frame."
  (interactive)
  (require 'etags)
  (point-to-mouse-maybe)
  (or default (setq default 
		    (funcall (or find-tag-default-function
				 (get major-mode 'find-tag-default-function)
				 'find-tag-default))))
  (let* ((pos (imenu---can-find-all-buffers default))
	 (pop-up-windows nil) 
	 ;; `let' does not work across buffer change for local variables
	 (old-c-f-s case-fold-search)
	 (b (current-buffer)))
    (setq imenu---stack (cons (point-marker) imenu---stack))
    (if pos (progn
	      (pop-to-buffer (car pos))
	      (goto-char (nth 1 pos)))
      (if (and (or
		(memq major-mode 
		      '(emacs-lisp-mode debugger-mode lisp-interaction-mode))
		(equal (buffer-name) "*Messages*"))
	       (fboundp 'find-function)
	       (intern-soft default)
	       (symbol-function (intern default))
	       (not (subrp (symbol-function (intern default))))
	       (condition-case nil
		   (find-function (intern default))
		 (error nil)))
	  nil
	(setq case-fold-search nil)
	(find-tag default)
	(save-excursion
	  (set-buffer b)
	  (setq case-fold-search old-c-f-s))))))

;;;###autoload
(defun imenu-go--back ()
  "Return back to a position saved during `imenu-go-find-at-position'."
  (interactive)
  (if (null imenu---stack)
      (error "No previous locations recorded"))
  (let ((marker (car imenu---stack)))
    ;; Pop the stack.
    (setq imenu---stack (cdr imenu---stack))
    (prog1
	;; Move to the saved location.
	(switch-to-buffer (marker-buffer marker))
      (goto-char (marker-position marker))
      ;; Kill that marker so it doesn't slow down editing.
      (set-marker marker nil nil))))

(provide 'imenu-go)
