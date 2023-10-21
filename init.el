;;; init.el -- The emacs config startup file

;;; Commentary:
;;; Code:

;; Load the `lisp` directory
(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "lisp")))

;; Load files
(require 'init-consts)
(require 'init-startup)
(require 'init-elpa)
(require 'init-package)
(require 'init-kbd)
(require 'init-ide)

(require 'init-ui)
;;; init.el ends here
