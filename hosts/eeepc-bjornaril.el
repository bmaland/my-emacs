(menu-bar-mode -1)
(add-to-list 'load-path "~/foss/emacs-w3m")
(require 'w3m-load)

;; REFACTOR
(global-set-key (kbd "C-c C-u a") (lambda () (interactive) (ucs-insert "00E5")))
(global-set-key (kbd "C-c C-u e") (lambda () (interactive) (ucs-insert "00E6")))
(global-set-key (kbd "C-c C-u o") (lambda () (interactive) (ucs-insert "00F8")))
;(global-set-key (kbd "C-c C-S-u A") (lambda () (interactive) (ucs-insert "00C5")))
