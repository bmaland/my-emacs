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

      (setq x-select-enable-clipboard t ;; merge Emacs kill-ring with the clipboard
            ;; open *help* in current frame
            special-display-regexps (remove "[ ]?\\*[hH]elp.*" special-display-regexps))

      (set-default-font "-apple-monaco-medium-r-normal--14-120-72-72-m-120-iso10646-1"))
  ;; CVS or Carbon Emacs
  (progn
    (setq mac-option-modifier 'none
          mac-command-modifier 'meta)

    ;; Fullscreen for Carbon Emacs
    (defun toggle-fullscreen ()
      (interactive)
      (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
                                               nil
                                             'fullboth)))

    (global-set-key [(meta return)] 'toggle-fullscreen)))
