;;; init.el -- The emacs config startup file

;;; Commentary:
;;; This file bootstraps the configuration, which is divided into a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;; (setq debug-on-error t)

;;; Check the minimum require version
(let (minver "26.1")
     (when (version< emacs-version minver)
       (error "Your Emacs is too old -- the config requires v%s or higher" minver)))
(when (version< emacs-version "27.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if posible."))

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

;; Allow access foe emacs client
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
