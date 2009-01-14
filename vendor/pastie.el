;; pastie.el -- Emacs integration for pastie.org.
;; Copyright (C) 2006 Christian Neukirchen <purl.org/net/chneukirchen>
;; Licensed under the GPL.
 
;; Version: 1.4
;; 08jan2007 +chris+
 
;; Thanks to: Phil Hagelberg for fixes.
 
(defun pastie-region (begin end)
  "Post the current region as a new paste at pastie.org.
Copies the URL into the kill ring."
  (interactive "r")
 
  (let* ((body-raw (buffer-substring begin end))
         (body (replace-regexp-in-string
                "[<>&]"
                (lambda (match)
                  (case (string-to-char match)
                    (?< "<")
                    (?> ">")
                    (?& "&")))
                body-raw))
 
         ;; Adjust as needed.
         (mode (or (cdr (assoc major-mode '((c-mode . "c")
                                            (c++-mode . "c")
                                            (diff-mode . "diff")
                                            (html-mode . "html")
                                            (javascript-mode . "javascript")
                                            (text-mode . "plaintext")
                                            (ruby-mode . "ruby")
                                            (sql-mode . "sql"))))
                   "plaintext"))
         
         (url-request-method "POST")
         (url-mime-accept-string "application/xml")
         (url-request-extra-headers '(("Content-Type" . "application/xml")))
         (url (url-generic-parse-url "http://pastie.org/pastes/create"))
 
         (url-request-data
          (concat "<paste>"
                  "<parser>" mode "</parser>"
                  "<body>" body "</body>"
                  "<authorization>burger</authorization>"
                  "</paste>")))
 
    (let ((pastie-buffer (url-retrieve-synchronously url)))
      (with-current-buffer pastie-buffer
        (goto-char (point-min))
        (search-forward-regexp "^Status: \\([0-9]+.*\\)")
        (let ((status (match-string 1)))
          (if (string-match "^20[01]" status)
              (progn
                (search-forward-regexp "^Location: \\(.*\\)")
                (beginning-of-line)
                (let ((result (match-string 1)))
                  (message "Paste created: %s" result)
                  (kill-new result)))
            (message "Error occured: %s" status))))
      (kill-buffer pastie-buffer))))
 
(defun pastie-buffer ()
  "Post the current buffer as a new paste at pastie.org.
Copies the URL into the kill ring."
  (interactive)
  (pastie-region (point-min) (point-max)))
 
(defun pastie-get (id)
  "Fetch the contents of the paste from pastie.org into a new buffer."
  (interactive "nPastie #: ")
 
  (let ((url-request-method "GET")
        (url-request-extra-headers nil)
        (url-mime-accept-string "*/*")
        (url (url-generic-parse-url
              (format "http://pastie.org/%s/download" id))))
    (setq pastie-buffer (url-retrieve-synchronously url))
 
    (with-current-buffer pastie-buffer
      (goto-char (point-min))
      (search-forward-regexp "^Status: \\([0-9]+.*\\)")
      (let ((status (match-string 1)))
        (if (string-match "^200" status)
            (progn
              (search-forward-regexp
               "^Content-Disposition: attachment; filename=\"\\(.*\\)\"")
              (set-visited-file-name (match-string 1))
              (search-forward-regexp "\n\n")
              (delete-region (point-min) (point))
              (normal-mode)
              (set-buffer-modified-p nil)
              (switch-to-buffer pastie-buffer))
          (message "Error occured: %s" status)
          (kill-buffer pastie-buffer))))))
 
(provide 'pastie)
;;; pastie.el ends here.
