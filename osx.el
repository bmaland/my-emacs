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

(setq browse-url-browser-function 'browse-url-default-macosx-browser)

;;(define-key dired-mode-map "o" 'dired-open-mac)
(defun dired-open-mac ()
       (interactive)
       (let ((file-name (dired-get-file-for-visit)))
         (if (file-exists-p file-name)
             (call-process "/usr/bin/open" nil 0 nil file-name))))

;; Implementation specific settings
(if (boundp 'aquamacs-version)
    ;; Aquamacs
    (progn
      (one-buffer-one-frame-mode 0)
      (emulate-mac-finnish-keyboard-mode t)
      (setq x-select-enable-clipboard t) ;; merge the Emacs kill-ring with the clipboard
      (set-default-font "-apple-monaco-medium-r-normal--14-120-72-72-m-120-iso10646-1"))
  ;; Carbon Emacs
  (setq mac-option-modifier 'meta))
