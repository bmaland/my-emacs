(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "black" :foreground "gray90" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :family "misc-fixed"))))
 '(tooltip ((((class color)) (:background "lightyellow" :foreground "black")))))


(load-library "/space/opt/xlestud/emacs/lfg-mode")
(setq load-path
      (cons "/space/opt/xlestud/emacs" load-path))

;; Use alt as meta
(setq x-alt-keysym 'meta
      visible-bell t)
