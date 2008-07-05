;; NOTE hooks that changes keybindings are in my-bindings.el

(add-hook 'shell-mode-hook
          '(lambda ()
             (setq sh-basic-offset 2)
             (setq indent-tabs-mode nil)))

(add-hook 'python-mode-hook
           '(lambda ()
              (setq indent-tabs-mode nil)
              (setq python-indent 4)))

(add-hook 'inferior-sml-load-hook
          '(lambda ()
             (load-library "sml-mosml")))

(provide 'my-hooks)
