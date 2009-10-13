 (defface egoge-display-time
   '((((type x w32 mac))
      (:foreground "#060525" :inherit bold))
     (((type tty))
      (:foreground "blue")))
   "Face used to display the time in the mode line.")

(defface paren-face
  '((((class color) (background dark))
     (:foreground "orange"))
    (((class color) (background light))
     (:foreground "orange")))
  "face used to color parentheses."
  :group 'my-faces)

(defface trailing-whitespace
   '((t (:background "pale green")))
   "Used for tabs and such.")

 (defface extra-whitespace-face
   '((t (:background "pale green")))
   "Used for tabs and such.")

(font-lock-add-keywords 'emacs-lisp-mode '(("(\\|)" . 'paren-face)))
(font-lock-add-keywords 'lisp-mode '(("(\\|)" . 'paren-face)))
(font-lock-add-keywords 'scheme-mode '(("(\\|)" . 'paren-face)))

(defvar my-extra-keywords
  '(("\t" . extra-whitespace-face)))

;; TODO this should be moved to my-hooks i think
(defvar programming-modes '(emacs-lisp-mode
                            lisp-mode
                            ruby-mode
                            python-mode
                            sh-mode
                            scheme-mode
                            js2-mode
                            sml-mode
                            ess-mode))

;; Highlight keywords for programming modes
(mapcar (lambda (mode)
          (font-lock-add-keywords
           mode
           '(("\\<\\(TODO\\|FIXME\\|FIX\\|HACK\\|REFACTOR\\|NOTE\\|OPTIMIZE\\)"
              1 font-lock-warning-face t))))
        programming-modes)

(mapcar (lambda (mode)
          (font-lock-add-keywords mode my-extra-keywords))
        programming-modes)

(provide 'my-faces)
