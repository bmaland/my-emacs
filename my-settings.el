(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

(setq user-full-name "Bjørn Arild Mæland"
      user-mail-address "bjorn.maeland@gmail.com"

      inhibit-startup-message t ;; Remove splash screen
      initial-major-mode 'emacs-lisp-mode ;; Elisp as default for scratch
      default-major-mode 'org-mode

      calendar-date-style 'european
      european-calendar-style t ;; Obsolute since 23.1, but keep for a while

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
      twittering-username "Chrononaut"
      display-time-string-forms '((propertize
                                   (concat " " 24-hours ":" minutes ", "
                                           day "." month " ")
                                   'face 'egoge-display-time)))

;; org-mode
(setq org-directory "~/notat"
      org-default-notes-file "~/notat/.notes"
      org-agenda-files '("~/notat")
      org-deadline-warning-days 7
      org-reverse-note-order nil
      org-log-done 'note
      org-return-follows-link t
      org-export-skip-text-before-1st-heading t
      org-export-with-LaTeX-fragments t
      org-export-html-style-extra "<style type=\"text/css\">
div.figure p { text-align: left; margin: 25px; }
</style>"
      org-file-apps '((auto-mode . emacs)
                      ("\\.x?html?\\'" . "firefox %s")
                      ("\\.pdf\\'" . "evince %s"))

      org-agenda-custom-commands
      '(("w" todo "WAITING"
         ((org-agenda-sorting-strategy '(priority-down))
          (org-agenda-prefix-format "  Mixed: ")))
        ("U" tags-tree "+boss-urgent"
         ((org-show-following-heading nil)
          (org-show-hierarchy-above nil)))
        ("N" search ""
         ((org-agenda-files '("~/notat"))
          (org-agenda-text-search-extra-files nil))))

      org-remember-templates
      '(("Todo" ?t "* TODO %^{Brief Description} %^g\nAdded: %U"
         "~/notat/gtd.org" "Usortert")

        ("Journal" ?j "\n* %^{topic} %T %^g \n%i%?\n"
         "~/notat/journal.org")
        )

      org-refile-targets '((nil . (:level . 1)) (nil . (:level . 2))
                           (("opensource.org" "webprojects.org") . (:tag . "PROJECT")))

      org-publish-project-alist
      '(
        ("studier-notes"
         :base-directory "~/notat/studier/"
         :base-extension "org"
         :publishing-directory "~/public_html/studier"
         :recursive nil
         :publishing-function org-publish-org-to-html
         :language "nn"
         :inline-images t
         :completion-function (lambda ()
                                (shell-command (concat "scp -r ~/notat/studier/* "
                                                       "rasmus.uib.no:~/public_html/studier/"))
         :headline-levels 4
         :auto-preamble t
         :auto-index t
         :index-title "Notat til emner ved UiB"
         )
        ("studier" :components ("studier-notes")))))

(setq remember-annotation-functions '(org-remember-annotation)
      remember-handler-functions '(org-remember-handler)

      ;; Jabber
      jabber-connection-type 'ssl
      jabber-server "gmail.com"
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
