;;; init-elpa.el -- Settings and helpers for package.el -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:

;; straight.el -- Next-generation, purely functional package manager for the Emacs hacker.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Integration straight.el with package.el
(setq straight-enable-package-integration t)

;; Set package center mirrors
(setq package-archives '(("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
			 ("gnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
			 ("org" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
			 ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")))

(setq package-check-signature nil) ; Don't check signatures

;;; Install into separate package dirs for each Emacs version, to prevent bytecode incompatibility
(setq package-user-dir
      (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                        user-emacs-directory))

(unless (bound-and-true-p package--initialized)
  (package-initialize))

(unless package-archive-contents
  (package-refresh-contents))

(straight-use-package 'use-package)

;; Use use-package to manage extensions
(setq straight-use-package-by-default t      ; Use straight as default package manager
      use-package-always-ensure t            ; Global ensure keyword
      use-package-always-defer t             ; Global defer load keyword
      use-package-always-demand nil
      use-package-expand-minimally t
      use-package-verbose t)

(provide 'init-elpa)
;;; init-elpa.el ends here
