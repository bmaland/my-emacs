# Instructions

## Aquamacs
Place (load "~/.emacs.d/init.el") in ~/Library/Preferences/Aquamacs Emacs/Preferences.el

## Byte compilation
Remember to byte compile:
(byte-recompile-directory "~/.emacs.d/vendor" 0)
(byte-recompile-directory "~/.emacs.d/elpa" 0)

NOTE: This might cause problems with SLIME.
