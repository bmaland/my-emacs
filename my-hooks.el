;; NOTE hooks that changes keybindings are in my-bindings.el

(add-hook 'sh-mode-hook
          '(lambda ()
             (make-local-variable 'write-contents-hooks)
             (add-hook 'write-contents-hooks 'untabify-buffer)
             (setq sh-basic-offset 2)
             (setq indent-tabs-mode nil)))

(add-hook 'python-mode-hook
          '(lambda ()
             (make-local-variable 'write-contents-hooks)
             (add-hook 'write-contents-hooks 'untabify-buffer)
             (setq indent-tabs-mode nil)
             (setq tab-width 4)
             (setq-default tab-width 4)
             (setq python-indent 4)
             (set (make-local-variable 'indent-tabs-mode) nil)))

(add-hook 'inferior-sml-load-hook
          '(lambda ()
             (load-library "sml-mosml")))

(add-hook 'markdown-mode-hook
          (lambda ()
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
