(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(require 'cl)
(setq user-full-name "Bjørn Arild Mæland"
      user-mail-address "bjorn.maeland@gmail.com"
      inhibit-startup-message t ;; Remove splash screen
      ispell-program-name "aspell"
      ispell-dictionary "english"
      dabbrev-case-replace nil ;; Make sure case is preserved
      bookmark-default-file "~/.emacs.d/bookmarks.bmk"
      bookmark-save-flag 1 ;; How many mods between saves
      scroll-margin 3
      scroll-conservatively 100
      c-basic-indent 2
      frame-title-format "emacs - %b"
      scroll-preserve-screen-position 1
      font-lock-maximum-decoration t
      slime-dir "~/foss/slime/"
      snippet-dir "~/foss/snippets/"
      server-window #'switch-to-buffer-other-frame
      display-time-string-forms '(
                                  (propertize
                                   (concat " " 24-hours ":" minutes ", " day "." month " ")
                                   'face 'egoge-display-time))
      org-log-done t
      org-return-follows-link t)

(setq-default fill-column 80 ;; how wide the screen should be before word wrapping
              indent-tabs-mode nil
              show-trailing-whitespace t
              tab-width 2)

(custom-set-variables
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(minibuffer-max-depth nil)        ;; enable multiple minibuffers
 '(case-fold-search t)              ;; case-insensitive search
 '(case-fold-search t)
 '(load-home-init-file t t)
 '(make-backup-files nil)
 '(pc-select-meta-moves-sexps t)
 '(pc-select-selection-keys-only t)
 '(pc-selection-mode t)
 '(iswitchb-mode t)
 '(require-final-newline t))

(transient-mark-mode t)
(show-paren-mode t)

(global-font-lock-mode t)

(when (bound-and-true-p window-system)
  (global-hl-line-mode t)
  (set-face-background 'hl-line "#232323"))

(if (fboundp 'blink-cursor-mode) (blink-cursor-mode 0)) ;; No blinking cursor!
(menu-bar-mode (if window-system 1 -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(fset 'yes-or-no-p 'y-or-n-p)

;; These are damn useful
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; display the current time
(display-time)

(server-start)

;; Load paths
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/sml-mode")

;; Slime
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

(defvar autosave-dir (concat "/tmp/." (user-login-name) "-emacs-autosaves/"))
(make-directory autosave-dir t)

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
          (if buffer-file-name
              (concat "#" (file-name-nondirectory buffer-file-name) "#")
            (expand-file-name (concat "#%" (buffer-name) "#")))))

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

(require 'pastie)
(require 'idle-highlight)
(require 'conservative-mode)
(require 'kill-wspace-mode)
(kill-wspace-mode 1)

;; Yasnippet
(when (file-directory-p snippet-dir)
  (require 'yasnippet)
  (require 'yasnippet-mode)
  (yas/initialize)
  (yas/load-directory "~/foss/snippets/"))

;; Personal customizations
(require 'my-faces)
(require 'my-elisp)
(require 'my-skeletons)
(require 'my-bindings)
(require 'my-aliases)
(require 'my-hooks)

(if (eq window-system 'mac)
    (load "~/.emacs.d/osx.el")
  (load "~/.emacs.d/linux.el"))

(setq system-specific-config
      (concat "~/.emacs.d/hosts/"
              (substring (shell-command-to-string "hostname -s") 0 -1) ".el"))

(if (file-exists-p system-specific-config)
    (load system-specific-config))

;; Display gtd when emacs starts, instead of *scratch*
(if (file-exists-p "~/notat/gtd.org") (find-file "~/notat/gtd.org"))
