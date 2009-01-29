(setq emms-browser-info-genre-format "%i● %n"
      emms-browser-info-artist-format "%i● %n"
      emms-browser-info-album-format "%i◎ %n"
      emms-browser-info-title-format "%i♪ %n")

(setq emms-last-played-format-alist
      '(((emms-last-played-seconds-today) . "Today at %H:%M")
        (604800                           . "%a at %H:%M")
        ((emms-last-played-seconds-month) . "%d")
        ((emms-last-played-seconds-year)  . "%m-%d")
        (t                                . "")))

(defun prettyfy-string (string &optional after)
  "Strip starting and ending whitespace and replace any chars
after AFTER with '...'"
  (let ((replace-map (list
                      (cons "^[ \t]*" "")
                      (cons "[ \t]*$" "")
                      (cons (concat "^\\(.\\{"
                                    (or (number-to-string after) "10")
                                    "\\}\\).*")
                            "\\1…"))))
    (dolist (replace replace-map)
      (when (string-match (car replace) string)
        (setq string (replace-match (cdr replace) nil nil string))))
    string))

(defun phil/emms-info-track-description (track)
  "Return a description of the current track."
  (let ((artist (emms-track-get track 'info-artist))
        (title (emms-track-get track 'info-title))
        (album (emms-track-get track 'info-album))
        (last-played (or (emms-track-get track 'last-played) '(0 0 0)))
        (play-count (or (emms-track-get track 'play-count) 0)))
    (format "%3d │ %-20s %-17s │ %-14s │ %-30s"
            play-count
            (prettyfy-string title 19)
            (prettyfy-string artist 16)
            (emms-last-played-format-date last-played)
            (prettyfy-string album 29))))

(setq emms-track-description-function
      'phil/emms-info-track-description)
