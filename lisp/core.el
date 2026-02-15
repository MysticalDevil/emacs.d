;;; core.el --- Package/bootstrap core -*- lexical-binding: t; -*-

;;; Commentary:
;; straight/use-package bootstrap and process environment setup.

;;; Code:

;; Prefer newer source files over stale byte-compiled artifacts.
(setq load-prefer-newer t)

(defvar bootstrap-version)
(defvar straight-base-dir)
(defvar exec-path-from-shell-variables)
(declare-function exec-path-from-shell-initialize "exec-path-from-shell")
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 7))
  ;; Keep startup alive when network/bootstrap is unavailable.
  (condition-case err
      (progn
        (unless (file-exists-p bootstrap-file)
          (with-current-buffer
              (url-retrieve-synchronously
               "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
               'silent 'inhibit-cookies)
            (goto-char (point-max))
            (eval-print-last-sexp)))
        (load bootstrap-file nil 'nomessage)
        (setq straight-use-package-by-default t
              use-package-expand-minimally t))
    (error
     (display-warning
      'core
      (format "straight bootstrap unavailable: %s" (error-message-string err))
      :warning))))

;; Make sure straight build artifacts are discoverable even when some repos are unavailable.
(let ((build-dir (expand-file-name "straight/build" user-emacs-directory)))
  (when (file-directory-p build-dir)
    (dolist (dir (directory-files build-dir t "^[^.]" t))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir)))))

;; Prefer locally available straight-managed `use-package'; fallback to system package.
(let* ((straight-repos-dir (and (boundp 'straight-base-dir)
                                (expand-file-name "repos" straight-base-dir)))
       (use-package-repo (and straight-repos-dir
                              (expand-file-name "use-package" straight-repos-dir))))
  (when (and (fboundp 'straight-use-package)
             use-package-repo
             (file-directory-p use-package-repo))
    (condition-case err
        (straight-use-package 'use-package)
      (error
       (display-warning
        'core
        (format "Failed activating straight use-package: %s" (error-message-string err))
        :warning)))))

(unless (require 'use-package nil 'noerror)
  (display-warning
   'core
   "use-package unavailable; package declarations are skipped"
   :warning)
  (defmacro use-package (&rest _args)
    "Fallback no-op when `use-package' is unavailable."
    nil))

;; Ensure user-local binaries are discoverable in all Emacs launch modes.
(defun my/prepend-dir-to-path (dir)
  "Prepend DIR to both `exec-path' and PATH when it exists."
  (when (and (stringp dir) (file-directory-p dir))
    (add-to-list 'exec-path dir)
    (unless (member dir (split-string (or (getenv "PATH") "") path-separator t))
      (setenv "PATH" (concat dir path-separator (or (getenv "PATH") ""))))))

(defun my/prepend-dev-bin-dirs-to-path ()
  "Prepend common user/dev binary directories to PATH and `exec-path'."
  (let* ((home (or (getenv "HOME") "~"))
         (gobin (getenv "GOBIN"))
         (gopath (getenv "GOPATH")))
    (dolist (dir (delq nil
                       (list (expand-file-name ".local/bin" home)
                             gobin
                             (and gopath (expand-file-name "bin" gopath))
                             ;; Keep support for custom Go root used on this machine.
                             (expand-file-name ".local/lib/go/bin" home))))
      (my/prepend-dir-to-path dir))))

(my/prepend-dev-bin-dirs-to-path)

;; Sync shell PATH and environment for GUI/daemon Emacs sessions.
(use-package exec-path-from-shell
  :if (or (daemonp) (display-graphic-p))
  :config
  (dolist (var '("PATH" "GOPATH" "GOBIN"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize)
  ;; Keep local dev bins available even if shell startup files omit them.
  (my/prepend-dev-bin-dirs-to-path))

(provide 'core)
;;; core.el ends here
