;;; etags-select-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (etags-select-find-tag etags-select-find-tag-at-point
;;;;;;  etags-select-mode-hook etags-select-no-select-for-one-match
;;;;;;  etags-select-mode) "etags-select" "etags-select.el" (18775
;;;;;;  64461))
;;; Generated autoloads from etags-select.el

(let ((loads (get 'etags-select-mode 'custom-loads))) (if (member '"etags-select" loads) nil (put 'etags-select-mode 'custom-loads (cons '"etags-select" loads))))

(defvar etags-select-no-select-for-one-match t "\
*If non-nil, don't open the selection window if there is only one
matching tag.")

(custom-autoload 'etags-select-no-select-for-one-match "etags-select" t)

(defvar etags-select-mode-hook nil "\
*List of functions to call on entry to etags-select-mode mode.")

(custom-autoload 'etags-select-mode-hook "etags-select" t)

(autoload 'etags-select-find-tag-at-point "etags-select" "\
Do a find-tag-at-point, and display all exact matches.  If only one match is
found, see the `etags-select-no-select-for-one-match' variable to decide what
to do.

\(fn)" t nil)

(autoload 'etags-select-find-tag "etags-select" "\
Do a find-tag, and display all exact matches.  If only one match is
found, see the `etags-select-no-select-for-one-match' variable to decide what
to do.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("etags-select-pkg.el") (18775 64461 761085))

;;;***

(provide 'etags-select-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; etags-select-autoloads.el ends here
