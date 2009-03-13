(defun coding-hook ()
  "Enable things that are convenient across all coding buffers."
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t) ;; Comments only
  (setq save-place t
        show-trailing-whitespace t)
  (local-set-key [tab] 'indent-or-expand))

(defun lisp-hook ()
  "Shared between lisp mode and emacs lisp mode"
  (local-set-key (kbd "C-c m") 'mark-sexp)

  (local-set-key [return] 'reindent-then-newline-and-indent)
  (local-set-key "\C-j" 'eval-print-last-sexp)
  (local-set-key "\M-." 'find-function)
  (local-set-key "\C-cp" 'slime-close-all-parens-in-sexp)

  (local-set-key [(meta up)] 'beginning-of-defun)
  (local-set-key [(meta down)] 'end-of-defun)

  (set-pairs '("(" "{" "[" "\"")))

(define-key lisp-mode-shared-map (kbd "C-\\") 'lisp-complete-symbol)
(define-key lisp-mode-shared-map (kbd "C-c v") 'eval-buffer)

(add-hook 'slime-repl-mode-hook
          (lambda ()
            (local-set-key "\C-cp" 'slime-close-all-parens-in-sexp)
            ))

(add-hook 'lisp-mode-hook
          '(lambda ()
             (coding-hook)
             (lisp-hook)
             (setq slime-complete-symbol*-fancy t)
             (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
             ))

(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (coding-hook)
             (lisp-hook)
             (eldoc-mode t)
             (make-local-variable 'after-save-hook)
             (add-hook
              'after-save-hook
              ;; If you're saving an elisp file, likely the .elc
              ;; is no longer valid.
              (lambda ()
                (if (file-exists-p (concat buffer-file-name "c"))
                    (delete-file (concat buffer-file-name "c")))))))

(add-hook 'clojure-mode-hook
          '(lambda ()
             (coding-hook)
             (lisp-hook)
             (require 'swank-clojure-autoload)
             ))

(add-hook 'c-mode-hook
          '(lambda ()
             (coding-hook)
             (set-pairs '("(" "<" "{" "[" "\"" "\'"))
             (c-set-style "GNU")
             (setq c-basic-offset 2)
             (local-set-key [return] 'reindent-then-newline-and-indent)
             (local-set-key "\C-c\C-z" 'compile)
             ))

(add-hook 'php-mode-hook
          '(lambda ()
             (coding-hook)
             (set-pairs '("<" "{" "[" "\"" "\'"))
             (setq c-basic-offset 4)
             (c-set-offset 'inline-open 0)
             (local-set-key [return] 'reindent-then-newline-and-indent)
             ))

(add-hook 'sh-mode-hook
          '(lambda ()
             (coding-hook)
             (set-pairs '("(" "{" "\"" "\'"))
             (setq sh-basic-offset 2)
             (local-set-key [return] 'reindent-then-newline-and-indent)
             (local-set-key "[" 'self-insert-command)
             ))

(add-hook 'js2-mode-hook
          '(lambda ()
             (coding-hook)
             (set-pairs '("(" "{" "[" "\"" "\'"))
             (setq js2-basic-offset 4)
             (define-key js2-mode-map (kbd "C-c l") 'js-lambda)
             (local-set-key [tab] 'js2-indent-line)
             ))

(add-hook 'prolog-mode-hook
          (lambda ()
            (coding-hook)
            (set-pairs '("(" "{" "[" "\"" "'"))
            ;;(modify-syntax-entry ?_ ".")
            (local-set-key [return] 'reindent-then-newline-and-indent)
            (local-set-key (kbd "C-c C-l") 'prolog-consult-file)
            (local-set-key (kbd "C-c C-z") 'run-prolog)))

(add-hook 'sml-mode-hook
          '(lambda ()
             (coding-hook)
             (set-pairs '("(" "{" "[" "\"" "\'"))
             (setq sml-electric-semi-mode t)
             (local-set-key [return] 'reindent-then-newline-and-indent)))

(add-hook 'inferior-sml-load-hook
          '(lambda ()
             (load-library "sml-mosml")))

(add-hook 'css-mode-hook
          '(lambda ()
             (set-pairs '("(" "[" "\"" "\'"))
             (setq css-indent-offset 2)
             (local-set-key [return] 'newline-and-indent)))

(add-hook 'haml-mode-hook
          '(lambda ()
             (set-pairs '("(" "[" "\"" "\'"))
             (local-set-key [return] 'newline-and-indent)))

(add-hook 'yaml-mode-hook
          '(lambda ()
             (set-pairs '("(" "[" "\"" "\'"))
             (local-set-key [return] 'newline-and-indent)))

(add-hook 'nxml-mode-hook
          '(lambda ()
             (set-pairs '("<" "{" "[" "\"" "\'"))
             (local-set-key [return] 'reindent-then-newline-and-indent)
             ))

(add-hook 'term-mode-hook
          '(lambda ()
             (set (make-local-variable 'scroll-margin) 0)))

(add-hook 'org-mode-hook
          (lambda ()
            (require 'bbdb)
            (bbdb-initialize)

            (set-pairs '("(" "{" "[" "\""))

            (local-set-key (kbd "M-s-<left>") 'org-promote-subtree)
            (local-set-key (kbd "M-s-<right>") 'org-demote-subtree)
            (local-set-key (kbd "S-<right>") 'windmove-right)
            (local-set-key (kbd "S-<left>") 'windmove-left)
            (local-set-key (kbd "S-<up>") 'windmove-up)
            (local-set-key (kbd "S-<down>") 'windmove-down)
            (local-set-key (kbd "C-c d") '(lambda () (interactive)
                                            (org-todo 'done)))
            (local-set-key (kbd "C-c s") '(lambda () (interactive)
                                            (org-todo "STARTED")))
            (local-set-key (kbd "C-c w") '(lambda () (interactive)
                                            (org-todo "WAITING")))

            (defun org-summary-todo (n-done n-not-done)
              "Switch entry to DONE when all subentries are done, to TODO otherwise."
              (let (org-log-done org-log-states)   ; turn off logging
                (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

            (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
            (auto-fill-mode 1)))

(add-hook 'remember-mode-hook 'org-remember-apply-template)

(add-hook 'markdown-mode-hook
          (lambda ()
            (set-pairs '("(" "{" "[" "\""))
            (auto-fill-mode 1)
            (setq tab-width 4)))

(add-hook 'text-mode-hook
          (lambda ()
            (set-pairs '("(" "{" "[" "\""))
            (auto-fill-mode 1)
            (bit-mode t)
            (setq tab-width 4)))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

(add-hook 'server-done-hook (lambda nil (kill-buffer nil)))

(eval-after-load "sql" '(progn (sql-set-product 'postgresql)))

(eval-after-load "slime"
  '(progn
     (slime-setup '(slime-fancy slime-asdf slime-banner))
     (setq slime-net-coding-system 'utf-8-unix)
     ))

(provide 'my-hooks)
