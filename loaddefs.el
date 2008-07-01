;;; loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (css-mode) "css-mode" "css-mode.el" (18507 63865))
;;; Generated autoloads from css-mode.el

(autoload 'css-mode "css-mode" "\
Major mode for editing CSS files

\(fn)" t nil)

;;;***

;;;### (autoloads (git-reblame git-blame-mode) "git-blame" "git-blame.el"
;;;;;;  (18507 54761))
;;; Generated autoloads from git-blame.el

(autoload 'git-blame-mode "git-blame" "\
Toggle minor mode for displaying Git blame

With prefix ARG, turn the mode on if ARG is positive.

\(fn &optional ARG)" t nil)

(autoload 'git-reblame "git-blame" "\
Recalculate all blame information in the current buffer

\(fn)" t nil)

;;;***

;;;### (autoloads (php-mode php-file-patterns) "php-mode" "php-mode.el"
;;;;;;  (18301 37295))
;;; Generated autoloads from php-mode.el

(defvar php-file-patterns '("\\.php[s34]?\\'" "\\.phtml\\'" "\\.inc\\'") "\
List of file patterns for which to automatically invoke `php-mode'.")

(custom-autoload 'php-file-patterns "php-mode" nil)

(autoload 'php-mode "php-mode" "\
Major mode for editing PHP code.

\\{php-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (ruby-mode) "ruby-mode" "ruby-mode.el" (18507 54625))
;;; Generated autoloads from ruby-mode.el

(autoload 'ruby-mode "ruby-mode" "\
Major mode for editing ruby scripts.
\\[ruby-indent-command] properly indents subexpressions of multi-line
class, module, def, if, while, for, do, and case statements, taking
nesting into account.

The variable ruby-indent-level controls the amount of indentation.
\\{ruby-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (rubydb) "rubydb2x" "rubydb2x.el" (18507 54625))
;;; Generated autoloads from rubydb2x.el

(autoload 'rubydb "rubydb2x" "\
Run rubydb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger.

\(fn COMMAND-LINE)" t nil)

;;;***

;;;### (autoloads (rubydb) "rubydb3x" "rubydb3x.el" (18507 54625))
;;; Generated autoloads from rubydb3x.el

(autoload 'rubydb "rubydb3x" "\
Run rubydb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger.

\(fn COMMAND-LINE)" t nil)

;;;***

;;;### (autoloads (twittering-mode) "twittering-mode" "twittering-mode.el"
;;;;;;  (18507 55564))
;;; Generated autoloads from twittering-mode.el

(autoload 'twittering-mode "twittering-mode" "\
Major mode for Twitter

\(fn)" t nil)

;;;***

;;;### (autoloads (yaml-mode) "yaml-mode" "yaml-mode.el" (18507 63549))
;;; Generated autoloads from yaml-mode.el

(autoload 'yaml-mode "yaml-mode" "\
Simple mode to edit YAML.

\\{yaml-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("git.el" "haml-mode.el" "inf-ruby.el"
;;;;;;  "init.el" "json.el" "markdown-mode.el" "my-bindings.el" "my-elisp.el"
;;;;;;  "pastie.el" "ruby-electric.el" "ruby-style.el" "sass-mode.el"
;;;;;;  "snippet.el" "textmate.el" "vc-git.el") (18535 23194 407554))

;;;***

(provide 'loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; loaddefs.el ends here
