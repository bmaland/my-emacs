;; TextMate behaviour on Emacs
;; Copyright (C) 2008  Orestis Markou

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
;;
;; You can file issues, send comments and get the latest version at:
;; http://code.google.com/p/emacs-textmate/
;;
;; Contributions welcome!
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-minor-mode textmate-mode
  "Toggle Textmate mode.
     With no argument, this command toggles the mode.
     Non-null prefix argument turns on the mode.
     Null prefix argument turns off the mode.
"
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " TM"
  ;; The minor mode bindings.
  '(
    ("\"" . move-over-dbl-quote)
    ("\'" . move-over-quote)
    (")" . move-over-bracket)
    ("]" . move-over-square)
    ("}" . move-over-curly)
    )
  :group 'textmate
  )

;;implementation stuff

(setq pushovers
      '(
        (?\" . (lambda () (forward-char 1) ))
        (?\' . (lambda () (forward-char 1) ))
        (?\) . (lambda () (up-list 1) ))
        (?\] . (lambda () (up-list 1) ))
        (?\} . (lambda () (up-list 1) ))
        ))

(setq defaults
      '(
        (?\" . (lambda () (skeleton-pair-insert-maybe nil) ))
        (?\' . (lambda () (skeleton-pair-insert-maybe nil) ))
        (?\) . (lambda () (insert-char ?\) 1) ))
        (?\] . (lambda () (insert-char ?\] 1) ))
        (?\} . (lambda () (insert-char ?\} 1) ))
        ))

(defun move-over (char)
  (if (eq (char-after) char)
      (funcall (cdr (assoc char pushovers)))
    (funcall (cdr (assoc char defaults)))
    )
  )


(defun move-over-bracket ()  (interactive)(move-over ?\) ))
(defun move-over-curly ()  (interactive)(move-over ?\} ))
(defun move-over-square ()  (interactive)(move-over ?\] ))
(defun move-over-quote ()  (interactive)(move-over ?\' ))
(defun move-over-dbl-quote ()  (interactive)(move-over ?\" ))

(provide 'textmate)
