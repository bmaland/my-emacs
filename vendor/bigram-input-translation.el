;;; bigram-input-translation.el --- translate (character) bigrams into a single character

;; Copyright (C) 2009 Bjørn Arild Mæland <bjorn.maeland@gmail.com>
;; License: GNU GPL (same license as Emacs)

;; Author: Bjørn Arild Mæland <bjorn.maeland@gmail.com>
;; Created: 22 Feb 2009
;; Version: 1.0
;; URL: http://chrononaut.net/
;; Keywords: keyboard international translation

;;; Commentary

;; I wrote this to be able to write Norwegian letters on an US keyboard.
;; On input it simply looks at the char before point and looks the combination
;; up in the bigram-translation-table. If found, the previous char is deleted
;; and instead of actual key pressed you get the translated char.

;; The default translation table is for Norwegian with the following
;; translations in place:
;; aa -> å
;; AA -> Å
;; ae -> æ
;; AE -> Æ
;; oe -> ø
;; OE -> Ø
(defvar norwegian-translation-table
  '(("a" . (lambda () (interactive) (maybe-insert-special-char '((97 . 229)))))
    ("A" . (lambda () (interactive) (maybe-insert-special-char '((65 . 197)))))
    ("e" . (lambda () (interactive) (maybe-insert-special-char '((111 . 248) (97 . 230)))))
    ("E" . (lambda () (interactive) (maybe-insert-special-char '((65 . 198) (79 . 216)))))))

(defvar bigram-translation-table norwegian-translation-table
  "Which translation table to use")

(defvar bit-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (binding bigram-translation-table)
      (define-key map (car binding) (cdr binding)))
    map))

(defun maybe-insert-special-char (from-to-list)
  (let ((key (cdr (assoc (char-before) from-to-list))))
    (if key
        (progn
          (backward-delete-char 1)
          (insert key))
      (self-insert-command 1))))

(define-minor-mode
  bit-mode
  "bigram input translation mode"
  :lighter " BIT")

(provide 'bit-mode)
