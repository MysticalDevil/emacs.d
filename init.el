;;; init.el --- The emacs config startup file

;;; Commentary:
;;; This file bootstraps the configuration, which is divided into a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;; (setq debug-on-error t)

;;; Check the minimum require version
(let ((minver "26.1"))
  (when (version< emacs-version minver)
    (error
     "Your Emacs is too old -- the config requires v%s or higher" minver)))
(when (version< emacs-version "27.1")
  (message
   "Your Emacs is old, and some functionality in this config
will be disabled. Please upgrade if posible."))

;; Load the `lisp` directory
(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "lisp")))

(defconst *spell-check-support-enabled* nil) ; Enable with t if you prefer
(defconst *is-mac* (eq system-type 'darwin))
(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *is-windows* (or
                        (eq system-type 'ms-dos) (eq system-type 'windows-nt)))

(defconst *ts-avaiable* (>= emacs-major-version 29))

;; Bootstrap config
(setq custom-file (locate-user-emacs-file "custom.el"))
(require 'init-site-lisp)
(require 'init-system)
(require 'init-funcs)
(require 'init-elpa)
(require 'init-evil)
(require 'init-package)
(require 'init-builtin)
(require 'init-prog)
(require 'init-completion)
(require 'init-ui)

(load-theme 'doom-one t)
(evil-mode 1)

;; Allow access foe emacs client
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

;; Variables configured via the interactive 'customize' interface
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
