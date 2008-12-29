(setq user-full-name "Bjørn Arild Mæland"
      user-mail-address "bjorn.maeland@gmail.com"

      inhibit-startup-message t ;; Remove splash screen
      initial-major-mode 'emacs-lisp-mode

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
      twittering-username "Chrononaut"
      display-time-string-forms '((propertize
                                   (concat " " 24-hours ":" minutes ", "
                                           day "." month " ")
                                   'face 'egoge-display-time))
      org-log-done t
      org-return-follows-link t

      ;; Jabber
      jabber-connection-type 'ssl
      jabber-server "gmail.com"
      jabber-network-server "talk.google.com"
      jabber-port 5223
      jabber-username "bjorn.maeland"

      ;; Files and paths
      bookmark-default-file "~/.emacs.d/bookmarks.bmk"
      bookmark-save-flag 1 ;; How many mods between saves
      slime-dir "~/foss/slime/"
      snippet-dir "~/.emacs.d/yasnippet/snippets/"
      autoload-file "~/.emacs.d/loaddefs.el"
      package-user-dir "~/.emacs.d/elpa"
      custom-file "~/.emacs.d/custom.el")


(setq-default fill-column 80 ;; how wide the screen should be before word wrapping
              indent-tabs-mode nil
              show-trailing-whitespace t
              tab-width 2)

(set-default 'imenu-auto-rescan t)
