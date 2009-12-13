;; Requirements:
;; Pymacs from http://pymacs.progiciels-bpi.ca/archives/Pymacs.tar.gz
;;
;; For flymake, pylint must be installed and the epylint wrapper must be
;; somewhere in PATH. On Arch Linux, epylint is included in the pylint
;; package. pylint can be installed with easy_install.
;;
;; Also, ropemacs:
;; http://www.enigmacurry.com/2009/01/21/autocompleteel-python-code-completion-in-emacs/

(setq interpreter-mode-alist (cons '("python" . python-mode)
                                   interpreter-mode-alist)

      comint-completion-autolist t  ;list possibilities on partial
          ;completion
      comint-completion-recexact nil  ;use shortest compl. if
          ;characters cannot be added
       ;; how many history items are stored in comint-buffers (e.g. py- shell)
       ;; use the HISTSIZE environment variable that shells use (if  avail.)
       ;; (default is 32)
      comint-input-ring-size (string-to-number (or (getenv
                                                    "HISTSIZE") "100")))

;;py-python-command-args '("-pylab"))

(setq pymacs-load-path '("~/.emacs.d/vendor"))

(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call  "pymacs")
(autoload 'pymacs-eval  "pymacs" nil t)
(autoload 'pymacs-exec  "pymacs" nil t)
(autoload 'pymacs-load  "pymacs" nil t)

(add-hook 'python-mode-hook
          '(lambda ()
             (coding-hook)
             ;(require 'ipython)
             (require 'pycomplete+)

             (eldoc-mode 1)
             (highlight-80+-mode t)

             ;; Initialize Rope
             ;; (when (not (fboundp 'ropemacs-mode))
             ;;   (pymacs-load "ropemacs" "rope-")
             ;;   (setq ropemacs-enable-autoimport 1))

             (c-subword-mode t)
             ;; (load-library "pylint")
             ;; (load "pylint-flymake.el")
             (set (make-variable-buffer-local 'beginning-of-defun-function)
                  'py-beginning-of-def-or-class)
             (set-pairs '("{" "[" "\"" "\'" "("))
             (setq py-indent-offset 4
                   py-shell-switch-buffers-on-execute nil
                   outline-regexp "def\\|class ")

             (local-set-key [return] 'py-newline-and-indent)
             (local-set-key (kbd "C-c C-z") 'py-shell)
             ;;(local-set-key "\t" 'ryan-python-tab)
             (local-set-key (kbd "C-h p") 'py-complete-help-thing-at-point)

             (local-set-key (kbd "C-;") 'my-insert-self)

             (when (and buffer-file-name
                        (file-writable-p
                         (file-name-directory buffer-file-name))
                        (file-writable-p buffer-file-name))
               (local-set-key (kbd "C-c d")
                              'flymake-display-err-menu-for-current-line)
               (flymake-mode t))))

(defun my-py-indent-def-or-class ()
  "Docstring"
  (interactive)
  (py-mark-def-or-class)
  (py-indent-region))

(defun my-insert-self ()
  "Insert self. at the beginning of the current expression."
  (interactive)
  (save-excursion
    (search-backward-regexp "[ \n\t,(-]\\|^")
    (if (not (looking-at "^"))
        (forward-char))
    (insert "self.")))

(defun py-next-block ()
   "go to the next block.  Cf. `forward-sexp' for lisp-mode"
   (interactive)
   (py-mark-block nil 't)
   (back-to-indentation))

;; NOTE Not sure if I really need this, py-complete works well enough I think.
;; Should be improved with completion etc
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auto-completion
;;;  Integrates:
;;;   1) Rope
;;;   2) Yasnippet
;;;   all with AutoComplete.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun prefix-list-elements (list prefix)
  (let (value)
    (nreverse
     (dolist (element list value)
       (setq value (cons (format "%s%s" prefix element) value))))))

(defvar ac-source-rope
  '((candidates
     . (lambda ()
         (prefix-list-elements (rope-completions) ac-target))))
  "Source for Rope")

(defun ac-python-find ()
  "Python `ac-find-function'."
  (require 'thingatpt)
  (let ((symbol (car-safe (bounds-of-thing-at-point 'symbol))))
    (if (null symbol)
        (if (string= "." (buffer-substring (- (point) 1) (point)))
            (point)
          nil)
      symbol)))

(defun ac-python-candidate ()
  "Python `ac-candidates-function'"
  (let (candidates)
    (dolist (source ac-sources)
      (if (symbolp source)
          (setq source (symbol-value source)))
      (let* ((ac-limit (or (cdr-safe (assq 'limit source)) ac-limit))
             (requires (cdr-safe (assq 'requires source)))
             cand)
        (if (or (null requires)
                (>= (length ac-target) requires))
            (setq cand
                  (delq nil
                        (mapcar (lambda (candidate)
                                  (propertize candidate 'source source))
                                (funcall (cdr (assq 'candidates source)))))))
        (if (and (> ac-limit 1)
                 (> (length cand) ac-limit))
            (setcdr (nthcdr (1- ac-limit) cand) nil))
        (setq candidates (append candidates cand))))
    (delete-dups candidates)))

(add-hook 'python-mode-hook
          (lambda ()
            (auto-complete-mode 1)
            (define-key ac-complete-mode-map "\C-n" 'ac-next)
            (define-key ac-complete-mode-map "\C-p" 'ac-previous)
            (local-set-key "\M-\C-q" 'my-py-indent-def-or-class)
            (local-set-key "\C-x s" 'my-insert-self)

            (set (make-local-variable 'ac-sources)
                 (append ac-sources '(ac-source-rope) '(ac-source-yasnippet)))
            (set (make-local-variable 'ac-find-function) 'ac-python-find)
            (set (make-local-variable 'ac-candidate-function) 'ac-python-candidate)
            (set (make-local-variable 'ac-auto-start) nil)))

;;Ryan's python specific tab completion
(defun ryan-python-tab ()
  ; Try the following:
  ; 1) Do a yasnippet expansion
  ; 2) Do a Rope code completion
  ; 3) Do an indent
  (interactive)
  (if (eql (ac-start) 0)
      (if (and
           (or (bobp) (= ?w (char-syntax (char-before))))
           (or (eobp) (not (= ?w (char-syntax (char-after))))))
          (dabbrev-expand arg) ;; havent confirmed that this works
        (indent-for-tab-command))))

(defadvice ac-start (before advice-turn-on-auto-start activate)
  (set (make-local-variable 'ac-auto-start) t))
(defadvice ac-cleanup (after advice-turn-off-auto-start activate)
  (set (make-local-variable 'ac-auto-start) nil))

(provide 'my-python)
