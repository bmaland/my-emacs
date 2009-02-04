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
(when (file-exists-p org-directory)
  (load (concat org-directory "/.settings.el")))

(setq inhibit-startup-message t ;; Remove splash screen
      initial-major-mode 'emacs-lisp-mode ;; Elisp as default for scratch
      default-major-mode 'org-mode

      calendar-date-style 'european

      ;; Flymake - only check syntax on save
      flymake-no-changes-timeout 9999
      flymake-start-syntax-check-on-newline nil
      flymake-start-syntax-check-on-find-file nil

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
      server-window #'switch-to-buffer-other-frame
      vc-follow-symlinks nil
      tramp-default-method "ssh"
      inferior-lisp-program "sbcl --no-linedit"
      multi-term-program "/bin/zsh"

      display-time-string-forms '((propertize
                                   (concat " " 24-hours ":" minutes ", "
                                           day "." month " ")
                                   'face 'egoge-display-time)))

(setq remember-annotation-functions '(org-remember-annotation)
      remember-handler-functions '(org-remember-handler)

      ;; Files and paths
      bookmark-default-file "~/.emacs.d/bookmarks.bmk"
      bookmark-save-flag 1 ;; How many mods between saves
      slime-dir "~/foss/slime/"
      snippet-dir "~/.emacs.d/yasnippet/snippets/"
      autoload-file "~/.emacs.d/loaddefs.el"
      package-user-dir "~/.emacs.d/elpa"
      custom-file "~/.emacs.d/custom.el")

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("ruby" (mode . ruby-mode))
               ("haml" (mode . haml-mode))
               ("elisp" (mode . emacs-lisp-mode))
               ("org" (mode . org-mode))
               ("erc" (mode . erc-mode))
               ("gtalk" (mode . jabber-chat-mode))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))))))

(setq-default fill-column 80 ;; how wide the screen should be before word wrapping
              indent-tabs-mode nil
              tab-width 2
              'imenu-auto-rescan t)
