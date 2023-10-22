;;; init-consts.el -- define some commonly used static constants

;;; Commentary:
;;; Code:

(defconst *is-mac* (eq system-type 'darwin))
(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *is-windows* (or (eq system-type 'ms-dos) (eq system-type 'windows-nt)))

(defconst *ts-avaiable* (>= emacs-major-version 29))

(provide 'init-consts)
;;; init-consts.el ends here
