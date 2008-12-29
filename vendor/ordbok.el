;;; -*- coding: mule-utf-8-unix -*-
;;; ordbok.el
;;; by: Kevin Brubeck Unhammer
;;; 
;;; Simple helper functions for Norwegian dictionary lookup. Handy
;;; when using Emacs for writing. Put this in your ~/.emacs:
;;
;; (autoload 'ordbok-oppslag "ordbok")
;; (autoload 'ordbok-slx-oppslag "ordbok")
;; (define-key global-map (kbd "C-$") 'ordbok-oppslag) ; since M-$ is ispell-word
;; (define-key global-map (kbd "C-M-$") 'ordbok-slx-oppslag) 
;;
;;; Alternatively, these keys are more ergonomic for Norwegian
;;; keyboards:
;; 
;; (define-key global-map (kbd "C-ø") 'ordbok-oppslag)  
;; (define-key global-map (kbd "C-M-ø") 'ordbok-slx-oppslag)  
;;
;;; To use ordbok-slx-oppslag, you need slx-dict. Get it from
;;; http://random.zerodogg.org/slx-dict
;;; 
;;; To use ordbok-oppslag, browse-url must work correctly. I recommend
;;; w3m as an in-emacs browser.

(defconst ordbok-version "2008-09-24")

(defvar ordbok-ignore-words "Location\\|msgstr\\|msgid"
  "Regexp of strings to ignore when picking up words from
  point.")

(defvar ordbok-slx-dict-program "slx-dict"
  "Path to slx-dict program. Get from
  http://random.zerodogg.org/slx-dict")

(defun ordbok-realword-at-point ()
  (unless (fboundp 'ispell-get-word) (load "ispell"))
  (let ((start (point))
	(word (car (ispell-get-word nil))))
    (goto-char start) 
    (if (string-match ordbok-ignore-words word)
	"" 
      word)))

(defun ordbok-oppslag ()
  "Look up a word in Bokmåls- og Nynorskordboka"
  (interactive)
  (let ((search-word 
	 (read-from-minibuffer "Ordbokoppslag: " 
			       (ordbok-realword-at-point))))
    (browse-url
     (concat "http://www.dokpro.uio.no/perl/ordboksoek/ordbok.cgi?ordbok=begge&s=n&alfabet=n&renset=j&OPP="
	     search-word))))

(defun ordbok-slx-oppslag ()
  "Translate English to Norwegian using Skolelinux'
Fellesordliste. (For reverse lookup, prepend with a ! eg. '!tjener')"
  (interactive)
  (let ((word 
	 (read-from-minibuffer "Translate (prepend Norwegian words by !): " 
			       (ordbok-realword-at-point))))
    (if (string= "!" (substring word 0 1)) 
	(setq word (concat "'" word "'"))) ; so that bash doesn't interpret the !
    (shell-command (concat ordbok-slx-dict-program " " word))))

(defun ordbok-slx-dict-update ()
  (interactive)
  (shell-command (concat ordbok-slx-dict-program " -o")))


(provide 'ordbok)
;;; ordbok.el ends here