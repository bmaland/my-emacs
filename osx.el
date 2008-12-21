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

;;(define-key dired-mode-map "o" 'dired-open-mac)
(defun dired-open-mac ()
       (interactive)
       (let ((file-name (dired-get-file-for-visit)))
         (if (file-exists-p file-name)
             (call-process "/usr/bin/open" nil 0 nil file-name))))

(defun toggle-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
                                           nil
                                           'fullboth)))

(global-set-key [(meta return)] 'toggle-fullscreen)

(if (boundp 'aquamacs-version)
    (load "~/.emacs.d/aquamacs.el")
  (setq mac-option-modifier 'meta))
