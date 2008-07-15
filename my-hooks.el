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

(add-hook 'slime-mode-hook
           (lambda ()
             (slime-highlight-edits-mode 0)))

(add-hook 'write-file-hooks 'nuke-trailing-whitespace)
;; Hopefully only temporary
(add-hook 'write-file-hooks 'untabify-buffer)

(provide 'my-hooks)
