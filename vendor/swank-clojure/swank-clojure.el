;;;; swank-clojure.el --- slime settings for clojure
;;;
;;; Copyright (C) 2008 Jeffrey Chu
;;;
;;; This file is licensed under the terms of the GNU General Public
;;; License as distributed with Emacs (press C-h C-c to view it).
;;;
;;; See swank-clojure.clj for instructions
;;;

(eval-and-compile
  (defvar swank-clojure-path
    (let ((path (file-truename (or (locate-library "swank-clojure")
                                   load-file-name))))
      (and path (file-name-directory path)))
    "Directory containing the swank-clojure package. This is used
to load the supporting clojure library swank."))

(defgroup swank-clojure nil
  "SLIME/swank support for clojure"
  :prefix "swank-clojure-"
  :group 'applications)

(defcustom swank-clojure-java-path "java"
  "The location of the java executable"
  :type 'string
  :group 'swank-clojure)

(defcustom swank-clojure-jar-path nil
  "The location of the jar file for clojure. For example,
/path/to/clojure.jar "
  :type 'string
  :group 'swank-clojure)

(defcustom swank-clojure-extra-classpaths (when (file-directory-p "~/.clojure") (directory-files "~/.clojure" t ".jar$"))
  "The classpath from which clojure will load from (passed into
java as the -cp argument). On default, it includes all jar files
within ~/.clojure/"
  :type 'list
  :group 'swank-clojure)

(defcustom swank-clojure-library-paths nil
  "The library paths used when loading shared libraries,
used to set the java.library.path property"
  :type 'list
  :group 'swank-clojure)

(defcustom swank-clojure-extra-vm-args nil
  "Extra arguments to be passed to the Java VM when starting clojure.
For example -Xmx512m or -Dsun.java2d.noddraw=true"
  :type 'list
  :group 'swank-clojure)

(defcustom swank-clojure-binary nil
  "Used as a binary executable (instead of
swank-clojure-java-path) if non-nil."
  :type 'string
  :group 'swank-clojure)

(defcustom swank-clojure-init-files nil
  "If provided, will be used to initialize the REPL environment."
  :type 'list
  :group 'swank-clojure)

(defcustom swank-clojure-compile-p nil
  "Whether or not to instruct swank-clojure to swank files. Set
  to nil if it's causing you problems."
  :type 'boolean
  :group 'swank-clojure)

(defface swank-clojure-dim-trace-face
  '((((class color) (background dark))
     (:foreground "grey50"))
    (((class color) (background light))
     (:foreground "grey55")))
  "Face used to dim parentheses."
  :group 'slime-ui)

(setq swank-clojure-dim-trace-face 'swank-clojure-dim-trace-face)

(defun swank-clojure-init (file encoding)
  (concat
   (when swank-clojure-compile-p
     "(require 'swank.loader)\n\n(swank.loader/init)\n\n")
   "(require 'swank.swank)\n\n"
   (when (boundp 'slime-protocol-version)
     (format "(swank.swank/ignore-protocol-version %S)\n\n" slime-protocol-version))
   (format "(swank.swank/start-server %S :encoding %S)\n\n"
           file (format "%s" encoding))))

(defun swank-clojure-find-package ()
  (let ((regexp "^(\\(clojure.core/\\)?\\(in-\\)?ns\\s-+[:']?\\([^()\" \t\n]+\\>\\)"))
    (save-excursion
      (when (or (re-search-backward regexp nil t)
                (re-search-forward regexp nil t))
        (match-string-no-properties 3)))))

(defun swank-clojure-slime-mode-hook ()
  (slime-mode 1)
  (set (make-local-variable 'slime-find-buffer-package-function) 'swank-clojure-find-package))

(defun swank-clojure-update-indentation (sym indent)
  (put sym 'clojure-indent-function indent))

(defun swank-clojure-concat-paths (paths)
  "Concatenate given list of `paths' using `path-separator'. (`expand-file-name'
will be used over paths too.)"
  (mapconcat 'identity (mapcar 'expand-file-name paths) path-separator))

(defun swank-clojure-cmd ()
  "Create the command to start clojure according to current settings."
  (if (and (not swank-clojure-binary) (not swank-clojure-jar-path))
      (error "You must specifiy either a `swank-clojure-binary' or a `swank-clojure-jar-path'")
    (if swank-clojure-binary
        (if (listp swank-clojure-binary)
            swank-clojure-binary
          (list swank-clojure-binary))
      (delete-if
       'null
       (append
        (list swank-clojure-java-path)
        swank-clojure-extra-vm-args
        (list
         (when swank-clojure-library-paths
           (concat "-Djava.library.path="
                   (swank-clojure-concat-paths swank-clojure-library-paths)))
         "-classpath"
         (swank-clojure-concat-paths
          (append (list swank-clojure-jar-path
                        swank-clojure-path)
                  swank-clojure-extra-classpaths))
         "clojure.main")
        (let ((init-opts '()))
          (dolist (init-file swank-clojure-init-files init-opts) 
            (setq init-opts (append init-opts (list "-i" init-file))))
          init-opts)
        (list "--repl"))))))

;; Change the repl to be more clojure friendly
(defun swank-clojure-slime-repl-modify-syntax ()
  (when (string-match "\\*slime-repl clojure\\*" (buffer-name))
    ;; modify syntax
    (modify-syntax-entry ?~ "'   ")
    (modify-syntax-entry ?, "    ")
    (modify-syntax-entry ?\{ "(}")
    (modify-syntax-entry ?\} "){")
    (modify-syntax-entry ?\[ "(]")
    (modify-syntax-entry ?\] ")[")
    (modify-syntax-entry ?^ "'")
    (modify-syntax-entry ?= "'")

    ;; set indentation function (already local)
    (setq lisp-indent-function 'clojure-indent-function)

    ;; set paredit keys
    (when (and (featurep 'paredit) paredit-mode (>= paredit-version 21))
      (define-key slime-repl-mode-map "{" 'paredit-open-curly)
      (define-key slime-repl-mode-map "}" 'paredit-close-curly))))

;; Debugger

(defun swank-clojure-dim-font-lock ()
  "Dim irrelevant lines in Clojure debugger buffers."
  (if (string-match "clojure" (buffer-name))
      (font-lock-add-keywords nil
                              '(("[0-9]+: \\(clojure\.\\(core\\|lang\\).*\\)"
                                 1 swank-clojure-dim-trace-face)
                                ("[0-9]+: \\(java.*\\)"
                                 1 swank-clojure-dim-trace-face)
                                ("[0-9]+: \\(swank.*\\)"
                                 1 swank-clojure-dim-trace-face)
                                ("\\[\\([A-Z]+\\)\\]"
                                 1 font-lock-function-name-face)))))

(add-hook 'sldb-mode-hook 'swank-clojure-dim-font-lock)

;; Importer

(defun swank-clojure-pick-import (classes)
  (swank-clojure-insert-import
   (list (if (and (boundp 'ido-mode) ido-mode)
             (ido-completing-read "Insert import: " classes)
           (completing-read "Insert import: " classes)))))

(defun swank-clojure-insert-import (classes)
  "Insert an :import directive in the ns macro to import full-class."
  (if (null classes) (error "No classes matched."))
  (if (= 1 (length classes))
      (save-excursion
        (goto-char (point-min))
        (search-forward "(ns ")
        (end-of-defun)
        (backward-char 2)
        (let* ((segments (split-string (first classes) "\\."))
               (package (mapconcat 'identity (butlast segments 1) "."))
               (class-name (car (last segments))))
          (insert (format "\n(:import [%s %s])" package class-name)))
        (indent-for-tab-command))
    (swank-clojure-pick-import classes)))

(defun swank-clojure-import (class)
  (interactive (list (read-from-minibuffer "Class: " (slime-symbol-at-point))))
  (slime-eval-async `(swank:classes-for ,class)
                    #'swank-clojure-insert-import))

(provide 'swank-clojure)
