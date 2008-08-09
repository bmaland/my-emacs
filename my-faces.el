(defface paren-face
  '((((class color) (background dark))
     (:foreground "orange"))
    (((class color) (background light))
     (:foreground "orange")))
  "face used to color parentheses."
  :group 'my-faces)

(font-lock-add-keywords 'emacs-lisp-mode '(("(\\|)" . 'paren-face)))
(font-lock-add-keywords 'lisp-mode '(("(\\|)" . 'paren-face)))
(font-lock-add-keywords 'scheme-mode '(("(\\|)" . 'paren-face)))
(mapcar (lambda (mode)
          (font-lock-add-keywords
           mode
           '(("\\<\\(TODO\\|FIXME\\|FIX\\|HACK\\|REFACTOR\\)"
              1 font-lock-warning-face t))))
        '(text-mode emacs-lisp-mode lisp-mode ruby-mode python-mode
                    sh-mode))

(provide 'my-faces)
