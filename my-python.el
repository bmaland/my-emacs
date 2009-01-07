;; Requirements:
;; Pymacs from http://pymacs.progiciels-bpi.ca/archives/Pymacs.tar.gz
;;
;; Then, copy pycomplete.py to somewhere in your Python path (site-packages)
;; http://groups.google.com/group/comp.lang.python/msg/048168c675ff0c68
;;
;; For flymake, pylint must be installed and the epylint wrapper must be
;; somewhere in PATH. On Arch Linux, epylint is included in the pylint
;; package.

(setq interpreter-mode-alist (cons '("python" . python-mode)
                                   interpreter-mode-alist))

(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call  "pymacs")
(autoload 'pymacs-eval  "pymacs" nil t)
(autoload 'pymacs-exec  "pymacs" nil t)
(autoload 'pymacs-load  "pymacs" nil t)

(add-hook 'python-mode-hook
          '(lambda ()
             (coding-hook)
             (require 'ipython)
             (require 'pycomplete)
             (c-subword-mode t)
             (load-library "pylint")
             (load "pylint-flymake.el")
             (set (make-variable-buffer-local 'beginning-of-defun-function)
                  'py-beginning-of-def-or-class)
             (setq outline-regexp "def\\|class ")
             ;; ( is handled by pycomplete.el
             (set-pairs '("{" "[" "\"" "\'"))
             (setq py-indent-offset 4
                   py-shell-switch-buffers-on-execute nil)
             (local-set-key [return] 'py-newline-and-indent)
             (local-set-key (kbd "C-c C-z") 'py-shell)

             (when (and buffer-file-name
                        (file-writable-p
                         (file-name-directory buffer-file-name))
                        (file-writable-p buffer-file-name))
               (local-set-key (kbd "C-c d")
                              'flymake-display-err-menu-for-current-line)
               (flymake-mode t))))

(defun my-python-documentation (w)
  "Launch PyDOC on the Word at Point"
  (interactive
   (list (let* ((word (thing-at-point 'word))
                (input (read-string
                        (format "pydoc entry%s: "
                                (if (not word) ""
                                  (format " (default %s)" word))))))
           (if (string= input "")
               (if (not word) (error "No pydoc args given")
                 word) ;sinon word
             input)))) ;sinon input
  (shell-command (concat py-python-command " -c \"from pydoc import help;help(\'" w "\')\"") "*PYDOCS*")
  (view-buffer-other-window "*PYDOCS*" t 'kill-buffer-and-window))

(defun py-next-block ()
  "go to the next block.  Cf. `forward-sexp' for lisp-mode"
  (interactive)
  (py-mark-block nil 't)
  (back-to-indentation))

(provide 'my-python)
