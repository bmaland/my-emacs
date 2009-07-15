(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:"
                       (getenv "HOME") "/dotfiles/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;;;;Private settings
;; Here stuff like names, usernames/passwords, etc, can be safely
;; stored outside of (public) version control.
(setq *private-settings* "~/.private.el")
(when (file-exists-p *private-settings*)
  (load *private-settings*))

;;;;;org-mode
;; Org-specific settings are kept in the root of my org-directory.
;; These has to be individually catered anyway so it doesnt really
;; make much sense to include them here.
(setq org-directory "~/notat")
(when (file-exists-p (concat org-directory "/.settings.el"))
  (load (concat org-directory "/.settings.el")))

;; Don't clutter up directories with files~
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        "~/.emacs.d/backups"))))

(setq x-select-enable-clipboard t)

;; do not turn on rng-validate-mode automatically
(setq rng-nxml-auto-validate-flag nil)

(setq prolog-system 'swi)

(setq inhibit-startup-message t ;; Remove splash screen
      initial-major-mode 'emacs-lisp-mode ;; Elisp as default for scratch
      default-major-mode 'org-mode

      calendar-date-style 'european

      ;; Flymake - only check syntax on save
      flymake-no-changes-timeout 9999
      flymake-start-syntax-check-on-newline nil
      flymake-start-syntax-check-on-find-file nil

      ido-ignore-buffers               ; ignore these guys
      '("\\` " "^\*Back" ".*Completion" "^\*Ido")

      ido-case-fold t                   ; be case insensitive
      ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point t
      ido-max-prospects 10

      ispell-program-name "aspell"
      ispell-dictionary "english"

      dabbrev-case-replace nil ;; Make sure case is preserved
      scroll-margin 3
      scroll-conservatively 100
      c-basic-indent 2
      frame-title-format "emacs - %b"
      scroll-preserve-screen-position 1
      font-lock-maximum-decoration t
      inhibit-default-init t
      visible-bell nil
      shift-select-mode nil
      browse-url-browser-function 'browse-url-firefox
      prolog-program-name "pl"
      vc-follow-symlinks nil
      tramp-default-method "ssh"
      inferior-lisp-program "sbcl --no-linedit"
      multi-term-program "/bin/zsh"

      display-time-string-forms '((propertize
                                   (concat " " 24-hours ":" minutes " ")
                                   'face 'egoge-display-time)))

(setq remember-annotation-functions '(org-remember-annotation)
      remember-handler-functions '(org-remember-handler)

      ;; Files and paths
      bookmark-default-file "~/.emacs.d/bookmarks.bmk"
      bookmark-save-flag 1 ;; How many mods between saves
      snippet-dir "~/.emacs.d/yasnippet/snippets/"
      custom-file "~/.emacs.d/custom.el")

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("ruby" (mode . ruby-mode))
               ("haml" (mode . haml-mode))
               ("elisp" (mode . emacs-lisp-mode))
               ("org" (mode . org-mode))
               ("erc" (mode . erc-mode))
               ("gtalk" (or
                         (mode . jabber-chat-mode)
                         (name . "*-jabber-roster-*")))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))))))

(setq-default fill-column 80 ;; how wide the screen should be before word wrapping
              indent-tabs-mode nil
              tab-width 2
              imenu-auto-rescan t)

;; Default minor modes
(yas/initialize)
(yas/load-directory snippet-dir)

(transient-mark-mode t)
(show-paren-mode t)
(savehist-mode t)
(global-font-lock-mode t)
(ido-mode t)
(recentf-mode t)
(display-time-mode t)
(auto-compression-mode t)
(kill-wspace-mode t)
(textmate-mode t)
(winner-mode t)

(if (fboundp 'blink-cursor-mode) (blink-cursor-mode 0))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(menu-bar-mode -1)

;; Functions
(fset 'yes-or-no-p 'y-or-n-p)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;;;Load more specific settings

(when (bound-and-true-p window-system)
  (require 'color-theme)
  (color-theme-initialize)
  (load "~/.emacs.d/vendor/color-theme/themes/color-theme-tango-2.el")
  (color-theme-tango-2)

  (global-hl-line-mode t)
  (set-face-background 'hl-line "#232323"))

;; Load OS-specific settings
(if (or (eq window-system 'mac) (eq window-system 'ns))
    (load "osx.el")
  (load "linux.el"))

;; Load Host-specific settings
(let ((system-specific-config
       (concat "~/.emacs.d/hosts/"
               (replace-regexp-in-string "\\..*" "" system-name)
               ".el")))
  (if (file-exists-p system-specific-config)
      (load system-specific-config)))

;; Personal customizations
(load custom-file 'noerror)
(require 'my-faces)
(require 'my-elisp)
(require 'my-bindings)
(require 'my-aliases)
(require 'my-ruby)
(require 'my-python)
(require 'my-hooks)
