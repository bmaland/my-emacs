;; NOTE hooks that changes keybindings are in my-bindings.el

(add-hook 'shell-mode-hook
          '(lambda ()
             (make-local-hook 'write-contents-hook)
             (add-hook 'write-contents-hooks 'untabify-buffer)
             (setq sh-basic-offset 2)
             (setq indent-tabs-mode nil)))

(add-hook 'python-mode-hook
          '(lambda ()
             (make-local-hook 'write-contents-hook)
             (add-hook 'write-contents-hooks 'untabify-buffer)
             (setq indent-tabs-mode nil)
             (setq python-indent 4)))

(add-hook 'inferior-sml-load-hook
          '(lambda ()
             (load-library "sml-mosml")))

(add-hook 'slime-mode-hook
          (lambda ()
            (slime-highlight-edits-mode 0)))

(add-hook 'write-file-hooks 'delete-trailing-whitespace)
(add-hook 'font-lock-mode-hook 'show-ws-highlight-tabs)

(provide 'my-hooks)
