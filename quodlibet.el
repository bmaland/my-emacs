;; quodlibet.el --- Control Quod Libet from GNU Emacs

;; Copyright (C) 2008 Bjørn Arild Mæland <bjorn.maeland@gmail.com>

;; Licensed under the same terms as Emacs.

;; Version: 0.1.0
;; Keywords: quod libet music player interface
;; Created: 8 Dec 2008
;; Author: Bjørn Arild Mæland <bjorn.maeland@gmail.com>

;; This file is NOT part of GNU Emacs.

;; Licensed under the same terms as Emacs.

;; Sample key bindings:

;; (global-set-key [f7] 'quodlibet-play)
;; (global-set-key [f8] 'quodlibet-pause)
;; (global-set-key [f9] 'quodlibet-prev)
;; (global-set-key [f10] 'quodlibet-next)
;; (global-set-key [f11] 'quodlibet-volume-down)
;; (global-set-key [f12] 'quodlibet-volume-up)

(defvar quodlibet-program-path "/home/bjorn/foss/quodlibet/quodlibet.py"
  "* Path to your quodlibet.py executable")

(defun quodlibet-next ()
  "Change to next track"
  (interactive)
  (shell-command (concat quodlibet-program-path " --next"))
  (message "Changed to next track"))

(defun quodlibet-prev ()
  "Change to previous track"
  (interactive)
  (shell-command (concat quodlibet-program-path " --prev"))
  (message "Changed to previous track"))

(defun quodlibet-stop ()
  "Stop playback"
  (interactive)
  (shell-command (concat quodlibet-program-path " --stop"))
  (message "Playback stopped"))

(defun quodlibet-pause ()
  "Paused playback"
  (interactive)
  (shell-command (concat quodlibet-program-path " --pause"))
  (message "Playback paused"))

(defun quodlibet-play ()
  "Start playback"
  (interactive)
  (shell-command (concat quodlibet-program-path " --play"))
  (message "Playback started"))

(defun quodlibet-show-window ()
  "Show Quod Libet's main window"
  (interactive)
  (shell-command (concat quodlibet-program-path " --show-window"))
  (message "Showing main window"))

(defun quodlibet-volume-up ()
  "Increase playback volume"
  (interactive)
  (shell-command (concat quodlibet-program-path " --volume-up"))
  (message "Increased playback volume"))

(defun quodlibet-volume-down ()
  "Decrease playback volume"
  (interactive)
  (shell-command (concat quodlibet-program-path " --volume-down"))
  (message "Decreased playback volume"))

(defun quodlibet-currently-playing ()
  "Display currently playing track and place it in the kill buffer"
  (interactive)
  (let ((current-track (quodlibet-current-track)))
    (kill-new current-track)
    (message (concat "Currently playing: " current-track))))

(defun quodlibet-set-rating ()
  "Rate the track that is currently playing"
  (interactive)
  (shell-command (concat quodlibet-program-path " --set-rating="
                         (read-string "Rate this track from 0.0 to 1.0: ")))
  (message (concat (quodlibet-current-track) " rated")))

;;; Utility

(defun quodlibet-current-track ()
  (chomp (shell-command-to-string (concat quodlibet-program-path " --print-playing"))))

(defun chomp (str)
  "Perl-like chomp function, trims whitespace"
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (save-excursion
      (while (and
              (not (null (string-match "^\\( \\|\f\\|\t\\|\n\\)" s)))
              (> (length s) (string-match "^\\( \\|\f\\|\t\\|\n\\)" s)))
        (setq s (replace-match "" t nil s)))
      (while (and
              (not (null (string-match "\\( \\|\f\\|\t\\|\n\\)$" s)))
              (> (length s) (string-match "\\( \\|\f\\|\t\\|\n\\)$" s)))
        (setq s (replace-match "" t nil s))))
    s))

(provide 'quodlibet)
;;; quodlibet.el ends here
