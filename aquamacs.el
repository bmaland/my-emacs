(one-buffer-one-frame-mode 0)
(emulate-mac-finnish-keyboard-mode t)

(defun writeroom ()
  "Switches to a WriteRoom-like fullscreen style"
  (interactive)	
  (when (featurep 'aquamacs)
					; switch to Garamond 36pt
    (set-frame-font "-apple-garamond-medium-r-normal--36-360-72-72-m-360-iso10646-1")
					; switch to white on black
    (color-theme-initialize)
    (color-theme-clarity)
					; switch to fullscreen mode
    (aquamacs-toggle-full-frame)))

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

;;(global-set-key (kbd “C-c j”) ‘flyspell-check-previous-highlighted-word)

