(setq rinari-tags-file-name "TAGS")

;; Code borrowed from Emacs starter kit

(defun rr (&optional arg)
  "Run a Ruby interactive shell session in a buffer."
  (interactive "P")
  (let ((impl (if (not arg)
                  "mri"
                (completing-read "Ruby Implementation: "
                                 '("ruby" "jruby" "rubinius" "yarv")))))
    (run-ruby (cdr (assoc impl '(("mri" . "irb")
                                 ("jruby" . "jruby -S irb")
                                 ("rubinius" . "rbx")
                                 ("yarv" . "irb1.9")))))
    (with-current-buffer "*ruby*"
      (rename-buffer (format "*%s*" impl) t))))

(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("jruby" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby1.9" . ruby-mode))

;;; rhtml mode
(add-to-list 'load-path "~/.emacs.d/vendor/rhtml")
(require 'rhtml-mode)

(add-hook 'rhtml-mode-hook
          (lambda () (rinari-launch)))

;; find-file-at-point help

(defun ruby-module-path (module)
    (shell-command-to-string
     (concat
      "ruby -e "
      "\"ret='()';$LOAD_PATH.each{|p| "
      "x=p+'/'+ARGV[0].gsub('.rb', '')+'.rb';"
      "ret=File.expand_path(x)"
      "if(File.exist?(x))};printf ret\" "
      module)))

(eval-after-load "ffap"
  '(push '(ruby-mode . ruby-module-path) ffap-alist))

;;; Rake

(defun pcomplete/rake ()
  (pcomplete-here (pcmpl-rake-tasks)))

(defun pcmpl-rake-tasks ()
   "Return a list of all the rake tasks defined in the current
projects.  I know this is a hack to put all the logic in the
exec-to-string command, but it works and seems fast"
   (delq nil (mapcar '(lambda(line)
      (if (string-match "rake \\([^ ]+\\)" line) (match-string 1 line)))
         (split-string (shell-command-to-string "rake -T") "[\n]"))))

(defun rake (task)
  (interactive (list (completing-read "Rake (default: default): "
                                      (pcmpl-rake-tasks))))
  (shell-command-to-string (concat "rake " (if (= 0 (length task)) "default" task))))

;; Clear the compilation buffer between test runs.
(eval-after-load 'ruby-compilation
  '(progn
     (defadvice ruby-do-run-w/compilation (before kill-buffer (name cmdlist))
       (let ((comp-buffer-name (format "*%s*" name)))
         (when (get-buffer comp-buffer-name)
           (with-current-buffer comp-buffer-name
             (delete-region (point-min) (point-max))))))
     (ad-activate 'ruby-do-run-w/compilation)))

;;; Flymake

(eval-after-load 'ruby-mode
  '(progn
     (require 'flymake)
     (require 'rinari)

     ;; Invoke ruby with '-c' to get syntax checking
     (defun flymake-ruby-init ()
       (let* ((temp-file (flymake-init-create-temp-buffer-copy
                          'flymake-create-temp-inplace))
              (local-file (file-relative-name
                           temp-file
                           (file-name-directory buffer-file-name))))
         (list "ruby" (list "-c" local-file))))

     (push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
     (push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)

     (push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3)
           flymake-err-line-patterns)

     (add-hook 'ruby-mode-hook
               (lambda ()
                 (c-subword-mode t)
                 (when (and buffer-file-name
                            (file-writable-p
                             (file-name-directory buffer-file-name))
                            (file-writable-p buffer-file-name))
                   (local-set-key (kbd "C-c d")
                                  'flymake-display-err-menu-for-current-line)
                   (flymake-mode t))))))

(provide 'my-ruby)
