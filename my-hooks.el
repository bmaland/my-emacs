;; NOTE hooks that changes keybindings are in my-bindings.el

(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (textmate-mode 1)
             ))

(add-hook 'sh-mode-hook
          '(lambda ()
             (make-local-variable 'write-contents-hooks)
             (add-hook 'write-contents-hooks 'untabify-buffer)
             (setq sh-basic-offset 2)
             ))

(add-hook 'python-mode-hook
          '(lambda ()
             (make-local-variable 'write-contents-hooks)
             (add-hook 'write-contents-hooks 'untabify-buffer)
             (setq python-indent 4)
             ))

(add-hook 'css-mode-hook
          '(lambda ()
             (make-local-variable 'write-contents-hooks)
             (add-hook 'write-contents-hooks 'untabify-buffer)
             (setq css-indent-offset 2)
             (setq css-electric-brace-behavior t)
             (setq css-electric-semi-behavior t)
             ))

(add-hook 'inferior-sml-load-hook
          '(lambda ()
             (load-library "sml-mosml")))

(add-hook 'markdown-mode-hook
          (lambda ()
            (textmate-mode 1)
            (auto-fill-mode 1)
            (setq tab-width 4)
            (make-local-variable 'write-contents-hooks)
            (add-hook 'write-contents-hooks 'untabify-buffer)))

(add-hook 'server-done-hook (lambda nil (kill-buffer nil)))

(eval-after-load "sql"
  '(progn
     (sql-set-product 'postgresql)
     ))

(add-hook 'write-file-hooks 'delete-trailing-whitespace)
(add-hook 'font-lock-mode-hook 'show-ws-highlight-tabs)

(provide 'my-hooks)
