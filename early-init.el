;;; early-init.el --- Emacs 27 introduces early-init.el, which runs before init.el

;;; Commentary:

;;; Emacs 27+ loads this file before (normally) calling
;;  `package-initialize`. We use this file to suppress that automatic
;;  behaviour so that startup is consistent across Emacs versions.

;;; Code:

;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package
;; initialization, so we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)
(setq load-prefer-newer noninteractive)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; System default coding
(set-language-environment 'utf-8)

;; Cleaner GUI
(unless (eq system-type 'darwin)
  (menu-bar-mode -1))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; So we can detect this having been loaded
(provide 'early-init)
;;; early-init.el ends here
