# Instructions

## Aquamacs
Place (load "~/.emacs.d/init.el") in ~/Library/Preferences/Aquamacs Emacs/Preferences.el

## Python support
Get Pymacs from http://pymacs.progiciels-bpi.ca/archives/Pymacs.tar.gz
and copy/symlink ~/.emacs.d/vendor/pycomplete.py to your PYTHONPATH.
You'll also need pylint (sudo easy_install pylint) + epylint in your PATH.

## Byte compilation
Remember to byte compile:
(byte-recompile-directory "~/.emacs.d/vendor" 0)
(byte-recompile-directory "~/.emacs.d/elpa" 0)
