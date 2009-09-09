;;; Fetched from http://gandalf.uib.no/lingkurs/webroot/static/shuffle-lines.el
;;; written by Paul Stoeber?

(defun swap-values (symbol1 symbol2)
  "Swap the values of SYMBOL1 and SYMBOL2.
Return the former value of SYMBOL1, the final value of SYMBOL2."
  (let ((x (symbol-value symbol1)))
    (set symbol1 (symbol-value symbol2))
    (set symbol2 x)))

(defun transpose-regions-allow-empty (startr1 endr1 startr2 endr2 &optional
                                              leave-markers)
  "Like transpose-regions, but allow empty regions."
  (if (> startr1 endr1)
      (swap-values 'startr1 'endr1))
  (if (> startr2 endr2)
      (swap-values 'startr2 'endr2))
  (if (> startr1 startr2)
      (progn (swap-values 'startr1 'startr2)
             (swap-values 'endr1 'endr2)))
  (if (= startr1 endr1)
      (setq endr1 startr2))
  (if (= startr2 endr2)
      (setq startr2 endr1))
  (if (and (/= startr1 endr1) (/= startr2 endr2))
      (transpose-regions startr1 endr1 startr2 endr2 leave-markers)))

(defun shuffle-regions (regions &optional leave-markers)
  "Randomly permute REGIONS given as a list of (BEG . END) cells.

The caller must ensure
 (i)   the regions don't overlap,
 (ii)  BEG is never greater than END,
 (iii) the regions are listed in the reverse order they appear in the buffer.

See `transpose-regions' for LEAVE-MARKERS."
  (let ((n (length regions)))
    (while (> n 1)
      (let ((r (random n)))
        (if (zerop r)
            (setq regions (cdr regions))
          (let* ((a (car regions)) (b (elt regions r))
                 (x (- (- (cdr a) (car a)) (- (cdr b) (car b)))))
            (transpose-regions-allow-empty
             (car a) (cdr a) (car b) (cdr b) leave-markers)
            (setq regions (cdr regions))
            (let ((iter regions))
              (while iter
                (let ((i (car iter)))
                  (if (eq i b)
                      (progn (setcdr i (+ x (cdr i)))
                             (setq iter nil))
                    (setcar i (+ x (car i)))
                    (setcdr i (+ x (cdr i)))))
                (setq iter (cdr iter)))))))
      (setq n (1- n)))))

(defun shuffle-lines (beg end)
  "Randomly permute lines in region."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let (lines)
  (while (not (eobp))
    (let ((beg (point)))
      (end-of-line)
      (let ((end (point)))
        (setq lines (cons (cons beg end) lines))))
    (forward-line))
  (shuffle-regions lines t)))))

(provide 'shuffle-lines)
