;; NOTE hooks that changes keybindings are in my-bindings.el

;; TODO investigate if these local hooks are needed on linux as well . .
;; I suspect I introduced the tab-problem during some Mac tweaking or similar

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

(add-hook 'slime-mode-hook
          (lambda ()
            (slime-highlight-edits-mode 0)))

(add-hook 'markdown-mode-hook
          (lambda ()
            (auto-fill-mode 1)
            (setq tab-width 4)
            (make-local-variable 'write-contents-hooks)
            (add-hook 'write-contents-hooks 'untabify-buffer)))

(add-hook 'write-file-hooks 'delete-trailing-whitespace)
(add-hook 'font-lock-mode-hook 'show-ws-highlight-tabs)

(provide 'my-hooks)
