;; Load the `lisp` directory
(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "lisp")))

;; Load files
(require 'init-startup)
(require 'init-elpa)
(require 'init-package)
(require 'init-ui)
