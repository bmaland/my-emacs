(require 'show-wspace)

(defun setup-show-ws ()
  (show-ws-highlight-tabs)
  (show-ws-highlight-trailing-whitespace))

(define-minor-mode
  kill-wspace-mode
  "Kill Wspace mode"
  :global t
  :lighter " kws"
  (if kill-wspace-mode
      (progn
        (add-hook 'write-file-hooks 'delete-trailing-whitespace)
        (add-hook 'write-file-hooks 'untabify-buffer)
        (add-hook 'font-lock-mode-hook 'setup-show-ws))
    (progn
      (remove-hook 'write-file-hooks 'delete-trailing-whitespace)
      (remove-hook 'write-file-hooks 'untabify-buffer)
      (remove-hook 'font-lock-mode-hook 'setup-show-ws))))

(add-hook 'kill-wspace-unload-hook (lambda () (kill-wspace-mode 0)))

(provide 'kill-wspace-mode)
