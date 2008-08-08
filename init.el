(setq user-full-name "Bjørn Arild Mæland"
      user-mail-address "bjorn.maeland@gmail.com")

(require 'cl) ;; Common Lisp compability

(setq inhibit-startup-message t) ;; Remove splash screen
(setq show-trailing-whitespace t)
(if (fboundp 'blink-cursor-mode) (blink-cursor-mode 0))

(setq ispell-program-name "aspell")
(setq ispell-dictionary "english")

;; show a menu only when running within X
(menu-bar-mode (if window-system 1 -1))

(prefer-coding-system 'utf-8)
(fset 'yes-or-no-p 'y-or-n-p)
;; These are damn useful
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(setq-default fill-column 80) ;; how wide the screen should be before word wrapping

(setq dabbrev-case-replace nil) ;; Make sure case is preserved
(setq bookmark-default-file "~/.emacs.d/bookmarks.bmk")
(setq bookmark-save-flag 1) ;; How many mods between saves

;; format the title-bar to always include the buffer name
(setq frame-title-format "emacs - %b")

;; Pgup/dn will return exactly to the starting point.
(setq scroll-preserve-screen-position 1)

;; display the current time
(display-time)

(server-start)

;; Load paths
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/sml-mode")

;; Slime
(setq slime-dir "~/foss/slime/")
(when (file-directory-p slime-dir)
  (add-to-list 'load-path slime-dir)
  (setq inferior-lisp-program "sbcl --no-linedit")

  (require 'slime)

  (eval-after-load "slime"
    '(progn
       (slime-setup '(slime-fancy slime-asdf slime-banner))
       (setq slime-complete-symbol*-fancy t)
       (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
       )))

;; Scroll margin and stop-that-bloody-halfpage-jump
(setq scroll-margin 3)
(setq scroll-conservatively 100)

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(global-hl-line-mode t)
(set-face-background 'hl-line "#8b4513")

(custom-set-variables
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(minibuffer-max-depth     nil)        ;; enable multiple minibuffers:
 '(show-paren-mode          t)          ;; match parens
 '(case-fold-search         t)          ;; case-insensitive search
 '(transient-mark-mode      t)          ;; highlight the marked region
 '(default-major-mode       'text-mode) ;; open unknown in text mode
 '(case-fold-search t)
 '(load-home-init-file t t)
 '(make-backup-files nil)
 '(pc-select-meta-moves-sexps t)
 '(pc-select-selection-keys-only t)
 '(pc-selection-mode t)
 '(require-final-newline t))

;; TODO move this stuff somewhere else
(defface paren-face
  '((((class color) (background dark))
     (:foreground "orange"))
    (((class color) (background light))
     (:foreground "orange")))
  "face used to color parentheses."
  :group 'my-faces)

(font-lock-add-keywords 'emacs-lisp-mode '(("(\\|)" . 'paren-face)))
(font-lock-add-keywords 'lisp-mode '(("(\\|)" . 'paren-face)))
(font-lock-add-keywords 'scheme-mode '(("(\\|)" . 'paren-face)))
(mapcar (lambda (mode)
          (font-lock-add-keywords
             mode
             '(("\\<\\(TODO\\|FIXME\\|FIX\\|HACK\\|REFACTOR\\)"
                1 font-lock-warning-face t))))
        '(text-mode emacs-lisp-mode lisp-mode ruby-mode python-mode))

(defvar autosave-dir (concat "/tmp/." (user-login-name) "-emacs-autosaves/"))
(make-directory autosave-dir t)

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
          (if buffer-file-name
              (concat "#" (file-name-nondirectory buffer-file-name) "#")
            (expand-file-name (concat "#%" (buffer-name) "#")))))

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(setq magic-mode-alist
      (cons '("<＼＼?xml " . nxml-mode)
            magic-mode-alist))
(fset 'xml-mode 'nxml-mode)

(setq tramp-default-method "ssh")

;; Regenerate the autoload file if it doesn't exist or it's too
;; old. (2 weeks or so)
(let ((autoload-file "~/.emacs.d/loaddefs.el"))
  (if (or (not (file-exists-p autoload-file))
          (< (+ (car (nth 5 (file-attributes autoload-file))) 20)
             (car (current-time))))
      (let ((generated-autoload-file autoload-file))
        (message "Updating autoloads...")
        (update-directory-autoloads "~/.emacs.d/")))
  (load autoload-file))

;; Autoloads
(autoload 'git-blame-mode "git-blame" "Minor mode for incremental blame for Git." t)

(autoload 'sml-mode "sml-mode" "Major mode for editing ML programs." t)
(setq auto-mode-alist
      (append '(("\\.sml$" . sml-mode)
                ("\\.sig$" . sml-mode)
                ("\\.ml$"  . sml-mode)
                ("\\.ML$"  . sml-mode)) auto-mode-alist))

(autoload 'w3m "w3m-load" "" t)
(autoload 'lisppaste-paste-region "lisppaste" "" t)

(autoload 'haml-mode "haml-mode" nil t)
(add-to-list 'auto-mode-alist '("\.haml$" . haml-mode))
(autoload 'sass-mode "sass-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))

(autoload 'php-mode "php-mode" "PHP Editing mode." t)
(add-to-list 'auto-mode-alist '("\.php$" . php-mode))

(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(autoload 'ruby-mode "ruby-mode" "Ruby editing mode." t)
(add-to-list 'auto-mode-alist '("\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
         (cons '("\\.text" . markdown-mode) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\.markdown$" . markdown-mode))

(autoload 'python-mode
  "python" "Python editing mode." t)

(autoload 'slime-selector "slime" t)

(require 'textmate)
(require 'pastie)
(require 'show-wspace)

;; I hate tabs!
(global-set-key (kbd "TAB") 'self-insert-command)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq tab-width 2)
(setq c-basic-indent 2)

;; Yasnippet

(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets/")

;; Personal customizations

(require 'my-elisp)
(require 'my-skeletons)
(require 'my-bindings)
(require 'my-aliases)
(require 'my-hooks)

(if (eq window-system 'mac)
    (load "~/.emacs.d/osx.el")
  (load "~/.emacs.d/linux.el"))

(setq system-specific-config
      (concat "~/.emacs.d/"
              (substring (shell-command-to-string "hostname") 0 -1) ".el"))

(if (file-exists-p system-specific-config)
    (load system-specific-config))

;; Display homedir when emacs starts, instead of *scratch*
(find-file "~/")
