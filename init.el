;; A bunch of this stuff is borrowed from Phil Hagelberg's personal dotfiles collection
;; over at http://github.com/technomancy/dotfiles/tree/master/

(setq inhibit-startup-message t) ;; Remove splash screen
(setq show-trailing-whitespace t)
(if (fboundp 'blink-cursor-mode) (blink-cursor-mode 0))
;; show a menu only when running within X (save real estate when
;; in console)
(menu-bar-mode (if window-system 1 -1))
(prefer-coding-system 'utf-8)
(fset 'yes-or-no-p 'y-or-n-p)
;; These are damn useful
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;(require 'cl)

;; format the title-bar to always include the buffer name
(setq frame-title-format "emacs - %b")

;; Pgup/dn will return exactly to the starting point.
(setq scroll-preserve-screen-position 1)

;; display the current time
(display-time)

;; Start server if not already running
(when (and (> emacs-major-version 22)
           (or (not (boundp 'server-process))
               (not (eq (process-status server-process)
                        'listen))))
  (server-start))

;; Load paths
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/sml-mode")
(add-to-list 'load-path "~/foss/slime/") ; your SLIME directory

;; (require 'vc-git)
;; (when (featurep 'vc-git) (add-to-list 'vc-handled-backends 'git))
;; (require 'git)

;; Slime
;;(setq inferior-lisp-program "sbcl --no-linedit")
;; (setq inferior-lisp-program "clisp")

;; (eval-after-load "slime"
;;   '(progn
;;      (slime-setup '(slime-fancy slime-asdf slime-banner))
;;      (setq slime-complete-symbol*-fancy t)
;;      (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)))

;; (require 'slime)
;; (slime-setup)

(defalias 'qrr 'query-replace-regexp)

;; ;; (autoload 'slime-selector "slime" t)

;; ;;(ad-activate 'indent-sexp)

;; (setq isearch-lazy-highlight nil)

;; Scroll margin and stop-that-bloody-halfpage-jump
;; (setq scroll-margin 3)
;; (setq scroll-step 0)
;; (setq scroll-conservatively 100)

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
 '(c-default-style          "k&r")      ;; why is this even an option?
 '(case-fold-search t)
 '(load-home-init-file t t)
 '(make-backup-files nil)
 '(pc-select-meta-moves-sexps t)
 '(pc-select-selection-keys-only t)
 '(pc-selection-mode t)
 '(require-final-newline t))

(defvar autosave-dir (concat "/tmp/." (user-login-name) "-emacs-autosaves/"))
(make-directory autosave-dir t)

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
          (if buffer-file-name
              (concat "#" (file-name-nondirectory buffer-file-name) "#")
            (expand-file-name (concat "#%" (buffer-name) "#")))))


;;(setq browse-url-browser-function 'browse-url-firefox)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(setq erc-server "irc.freenode.net"
      erc-port 6667
      erc-nick "Chrononaut-"
      erc-user-full-name "Bjørn Arild Mæland"
      erc-email-userid "chrononaut"    ; for when ident is not activated
      erc-prompt-for-password nil) ; OPN doesn't require passwords

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

(autoload 'php-mode "php-mode" "PHP Editing mode." t)
(add-to-list 'auto-mode-alist '("\.php$" . php-mode))

(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(autoload 'ruby-mode "ruby-mode" "Ruby editing mode." t)
(add-to-list 'auto-mode-alist '("\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
         (cons '("\\.text" . markdown-mode) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\.markdown$" . markdown-mode))

(autoload 'python-mode
  "python" "Python editing mode." t)

(require 'textmate)
(textmate-mode)
(require 'pastie)
(require 'show-wspace)

;; I hate tabs!
(global-set-key (kbd "TAB") 'self-insert-command)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq tab-width 2)
(setq c-basic-indent 2)

;; Personal customizations

(require 'my-elisp)
(require 'my-bindings)
(require 'my-hooks)

(if (eq window-system 'mac)
    (load "~/.emacs.d/osx.el")
  (load "~/.emacs.d/linux.el"))

(setq system-specific-config
      (concat "~/.emacs.d/"
              (substring (shell-command-to-string "hostname") 0 -1) ".el"))

(if (file-exists-p system-specific-config)
    (load system-specific-config))
