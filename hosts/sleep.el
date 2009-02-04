;; EMMS
(require 'my-emms)

;; Setup file cache
(eval-after-load
    "filecache"
  '(progn
     (message "Loading file cache...")
     (file-cache-add-directory-list load-path)
     (file-cache-add-directory "~/.zsh")
     (file-cache-add-directory "~/notat")
     (file-cache-add-directory "~/dotfiles")
     ))
