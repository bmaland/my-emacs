(one-buffer-one-frame-mode 0)
(emulate-mac-finnish-keyboard-mode t)

;;(define-key dired-mode-map "o" 'dired-open-mac)
(defun dired-open-mac ()
       (interactive)
       (let ((file-name (dired-get-file-for-visit)))
         (if (file-exists-p file-name)
             (call-process "/usr/bin/open" nil 0 nil file-name))))

(setq initial-frame-alist '((background-color . "black")
                            (foreground-color . "white")
                            (cursor-color . "LightSkyBlue")
                            (mouse-color . "LightSkyBlue")
                            (left . 50)  ))
(setq default-frame-alist '((background-color . "black")
                            (foreground-color . "white")
                            (cursor-color . "LightSkyBlue")
                            (mouse-color . "LightSkyBlue")
                            (left . 0) (width . 141) (height . 44)))

(setq x-select-enable-clipboard t) ;; merge the Emacs kill-ring with the clipboard

;;(global-set-key (kbd “C-c j”) ‘flyspell-check-previous-highlighted-word)
