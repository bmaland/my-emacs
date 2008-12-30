;;; ack.el --- grep like compilation mode for Ack
;; Original code from http://www.rooijan.za.net/?q=node/530
;; It has since been enhanced by Bjørn Arild Mæland

(require 'compile)
(require 'thingatpt)

(defvar ack-command "ack --nogroup --no-color"
  "The command run by the ack function.")

(defvar ack-mode-font-lock-keywords
  '(("^\\(Compilation\\|Ack\\) started.*"
     (0 '(face nil message nil help-echo nil mouse-face nil) t))))

(defvar ack-use-search-in-buffer-name t
  "If non-nil, use the search string in the ack buffer's name.")

(define-derived-mode ack-mode grep-mode "ack")

(defun ack (dir pattern args)
  "Run ack, with user-specified ARGS, and collect output in a buffer.
While ack runs asynchronously, you can use the \\[next-error] command to
find the text that ack hits refer to. The command actually run is
defined by the ack-command variable."
  (interactive (list (read-file-name "Run ack in directory: " nil "" t)
                     (read-string "Search for: " (thing-at-point 'symbol))
                     (read-string "Ack arguments: " "-i" nil "-i" nil)
                                  ))
  ; Get dir into an the right state, incase a file name was used
    (setq dir (abbreviate-file-name
               (file-name-as-directory (expand-file-name dir))))
    ;; Check that it's really a directory.
    (or (file-directory-p dir)
        (error "ack needs a directory: %s" dir))

  (let (compile-command
        (compilation-error-regexp-alist grep-regexp-alist)
        (compilation-directory default-directory)
        (ack-full-buffer-name (concat "*ack-" pattern "*")))
    ;;    (save-some-buffers (not compilation-ask-about-save) nil)
    ;; lambda defined here since compilation-start expects to call a function to get the buffer name
    (compilation-start (concat ack-command " " args " " pattern " " dir) 'ack-mode
                       (when ack-use-search-in-buffer-name
                         (function (lambda (ignore)
                                     ack-full-buffer-name)))
                       (regexp-quote pattern))))

;; Two strategies are used to find the project root. If the magit.el library
;; is loaded, magit-get-top-dir will be used. If this is not available, or the
;; function returns nil, the Emacs 23 feature "dir-local variables" will be used.
;; This feature traverses the directory hierarchy for a file called ".dir-locals.el".
(defun ack-in-project (pattern)
  (interactive (list (read-string "Search for: " (thing-at-point 'symbol))))
  (let* ((git-dir (when (functionp 'magit-get-top-dir)
                    (magit-get-top-dir (file-name-directory (buffer-file-name)))))
         (dir
          (if git-dir
              git-dir
            (first (dir-locals-find-file (buffer-file-name))))))
    (if dir
        (ack dir pattern "")
      (message (concat "Project file not found: " dir-locals-file)))))

(provide 'ack)
