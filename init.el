(prefer-coding-system 'utf-8)
(random t)

;; Load paths
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/vendor")
(add-to-list 'load-path "~/.emacs.d/vendor/sml-mode")
(add-to-list 'load-path "~/.emacs.d/vendor/clojure-mode")
(add-to-list 'load-path "~/.emacs.d/vendor/swank-clojure")
(add-to-list 'load-path "~/.emacs.d/vendor/ri-emacs")
(add-to-list 'load-path "~/.emacs.d/vendor/org-mode")
(add-to-list 'load-path "~/.emacs.d/vendor/bbdb")
(add-to-list 'load-path "~/.emacs.d/vendor/slime")
(add-to-list 'load-path "~/.emacs.d/vendor/jabber")
(add-to-list 'load-path "~/.emacs.d/vendor/color-theme")
(add-to-list 'load-path "~/.emacs.d/vendor/ess/lisp")
(add-to-list 'load-path "~/.emacs.d/vendor/w3m")
(add-to-list 'load-path "~/.emacs.d/vendor/rspec-mode")
(add-to-list 'load-path "~/.emacs.d/elpa")

;; TODO autoload some of these
(require 'cl)
(require 'browse-kill-ring)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)
(require 'pastie)
(require 'conservative-mode)
(require 'kill-wspace-mode)
(require 'ack)
(require 'filecache)
(require 'textmate)
(require 'ri)
(require 'multi-term)
(require 'slime)
(require 'yasnippet)
(require 'yasnippet-mode)
(require 'color-theme-autoloads "color-theme-autoloads")
(require 'ess-site)

;; ELPA
(require 'package)
(package-initialize)

;; I had to do this to use python-mode.el over python.el
(load "vendor/python-mode.el")

;; Autoloads
(autoload 'bit-mode "bigram-input-translation" nil t)
(autoload 'google-define "google-define" nil t)
(autoload 'rspec-mode "rspec-mode" nil t)
(autoload 'shuffle-lines "shuffle-lines" nil t)
(autoload 'slime-selector "slime" t)
(autoload 'sr-speedbar-toggle "sr-speedbar")
(autoload 'twittering-mode "twittering-mode" nil t)
(autoload 'w3m-load "w3m" t)

(autoload 'clojure-mode "clojure-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")

(autoload 'git-blame-mode "git-blame" "Minor mode for incremental blame for Git." t)

(autoload 'sml-mode "sml-mode" "Major mode for editing ML programs." t)
(setq auto-mode-alist
      (append '(("\\.sml$" . sml-mode)
                ("\\.sig$" . sml-mode)
                ("\\.ml$"  . sml-mode)
                ("\\.ML$"  . sml-mode)) auto-mode-alist))

(autoload 'w3m "w3m-load" "" t)
(autoload 'lisppaste-paste-region "lisppaste" "" t)

(autoload 'auto-complete-mode "auto-complete")

(autoload 'doctest-mode "doctest-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.doctest$" . doctest-mode))

(autoload 'haml-mode "haml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))
(autoload 'sass-mode "sass-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))

(autoload 'php-mode "php-mode" "PHP Editing mode." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

(autoload 'yaml-mode "yaml-mode" "YAML Editing mode." t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(autoload 'octave-mode "octave-mod" nil t)
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))

(autoload 'ruby-mode "ruby-mode" "Ruby editing mode." t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.sake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.builder$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

(add-to-list 'auto-mode-alist '("_spec.rb$" . rspec-mode))
(add-to-list 'auto-mode-alist '("_behavior.rb$" . rspec-mode))

(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)

(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)
                                ("\\.dcg$" . prolog-mode))
                               auto-mode-alist))

(add-to-list 'auto-mode-alist '("\.org$" . org-mode))

(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
         (cons '("\\.text" . markdown-mode) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\.md$" . markdown-mode))

;; Regenerate the autoload file if it doesn't exist or it's too
;; old. (2 weeks or so)
;; NOTE is this really needed anymore?
(setq autoload-file "~/.emacs.d/loaddefs.el")
(if (or (not (file-exists-p autoload-file))
        (< (+ (car (nth 5 (file-attributes autoload-file))) 20)
           (car (current-time))))
    (let ((generated-autoload-file autoload-file))
      (message "Updating autoloads...")
      (update-directory-autoloads "~/.emacs.d/")))
(load autoload-file)

(load "my-settings.el")
