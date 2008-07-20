;;; loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (css-mode) "css-mode" "css-mode.el" (18538 39383))
;;; Generated autoloads from css-mode.el

(autoload (quote css-mode) "css-mode" "\
Major mode for editing CSS files

\(fn)" t nil)

;;;***

;;;### (autoloads (git-reblame git-blame-mode) "git-blame" "git-blame.el"
;;;;;;  (18538 39383))
;;; Generated autoloads from git-blame.el

(autoload (quote git-blame-mode) "git-blame" "\
Toggle minor mode for displaying Git blame

With prefix ARG, turn the mode on if ARG is positive.

\(fn &optional ARG)" t nil)

(autoload (quote git-reblame) "git-blame" "\
Recalculate all blame information in the current buffer

\(fn)" t nil)

;;;***

;;;### (autoloads (php-mode php-file-patterns) "php-mode" "php-mode.el"
;;;;;;  (18538 39383))
;;; Generated autoloads from php-mode.el

(defvar php-file-patterns (quote ("\\.php[s34]?\\'" "\\.phtml\\'" "\\.inc\\'")) "\
List of file patterns for which to automatically invoke `php-mode'.")

(custom-autoload (quote php-file-patterns) "php-mode" nil)

(autoload (quote php-mode) "php-mode" "\
Major mode for editing PHP code.

\\{php-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (ruby-mode) "ruby-mode" "ruby-mode.el" (18538 39383))
;;; Generated autoloads from ruby-mode.el

(autoload (quote ruby-mode) "ruby-mode" "\
Major mode for editing ruby scripts.
\\[ruby-indent-command] properly indents subexpressions of multi-line
class, module, def, if, while, for, do, and case statements, taking
nesting into account.

The variable ruby-indent-level controls the amount of indentation.
\\{ruby-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (rubydb) "rubydb2x" "rubydb2x.el" (18538 39383))
;;; Generated autoloads from rubydb2x.el

(autoload (quote rubydb) "rubydb2x" "\
Run rubydb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger.

\(fn COMMAND-LINE)" t nil)

;;;***

;;;### (autoloads (rubydb) "rubydb3x" "rubydb3x.el" (18538 39383))
;;; Generated autoloads from rubydb3x.el

(autoload (quote rubydb) "rubydb3x" "\
Run rubydb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger.

\(fn COMMAND-LINE)" t nil)

;;;***

;;;### (autoloads (show-ws-toggle-show-trailing-whitespace show-ws-toggle-show-hard-spaces
;;;;;;  show-ws-toggle-show-tabs) "show-wspace" "show-wspace.el"
;;;;;;  (18299 241))
;;; Generated autoloads from show-wspace.el

(defalias (quote toggle-show-tabs-show-ws) (quote show-ws-toggle-show-tabs))

(autoload (quote show-ws-toggle-show-tabs) "show-wspace" "\
Toggle highlighting of TABs, using face `show-ws-tab'.

\(fn)" t nil)

(defalias (quote toggle-show-hard-spaces-show-ws) (quote show-ws-toggle-show-hard-spaces))

(autoload (quote show-ws-toggle-show-hard-spaces) "show-wspace" "\
Toggle highlighting of non-breaking space characters (`\240').
Uses face `show-ws-hard-space'.

\(fn)" t nil)

(defalias (quote toggle-show-trailing-whitespace-show-ws) (quote show-ws-toggle-show-trailing-whitespace))

(autoload (quote show-ws-toggle-show-trailing-whitespace) "show-wspace" "\
Toggle highlighting of trailing whitespace.
Uses face `show-ws-trailing-whitespace'.

\(fn)" t nil)

;;;***

;;;### (autoloads (twittering-mode) "twittering-mode" "twittering-mode.el"
;;;;;;  (18538 39383))
;;; Generated autoloads from twittering-mode.el

(autoload (quote twittering-mode) "twittering-mode" "\
Major mode for Twitter

\(fn)" t nil)

;;;***

;;;### (autoloads (yaml-mode) "yaml-mode" "yaml-mode.el" (18538 39383))
;;; Generated autoloads from yaml-mode.el

(autoload (quote yaml-mode) "yaml-mode" "\
Simple mode to edit YAML.

\\{yaml-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("aquamacs.el" "cheat.el" "git.el" "haml-mode.el"
;;;;;;  "idle-highlight.el" "inf-ruby.el" "init.el" "json.el" "linux.el"
;;;;;;  "lisppaste.el" "markdown-mode.el" "my-bindings.el" "my-elisp.el"
;;;;;;  "my-hooks.el" "osx.el" "pastie.el" "ruby-electric.el" "ruby-style.el"
;;;;;;  "sass-mode.el" "snippet.el" "textmate.el" "vc-git.el" "zero.el")
;;;;;;  (18560 39101 39785))

;;;***

(provide 'loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; loaddefs.el ends here
