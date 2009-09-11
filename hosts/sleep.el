;; Clojure
(setq swank-clojure-jar-path "/home/bjorn/java/clojure.jar")
(setq swank-clojure-extra-classpaths '("/home/bjorn/java"))
;; yasnippet dev
(setq snippet-dir "~/src/snippets")
(yas/load-directory snippet-dir)
;; etc
(setq browse-url-generic-program "chromium-browser")


;; Imaxima
(push "/home/bjorn/src/imaxima-imath-1.0" load-path)

;;; add autoload of imaxima and maxima.
(autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
(autoload 'maxima "maxima" "Frontend for maxima" t)
;;; add autoload of imath.
(autoload 'imath-mode "imath" "Imath mode for math formula input" t)

(setq imaxima-fnt-size "Large")
