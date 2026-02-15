;;; core.el --- Package/bootstrap core -*- lexical-binding: t; -*-

;; Prefer newer source files over stale byte-compiled artifacts.
(setq load-prefer-newer t)

(defvar bootstrap-version)
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

;; Ensure `use-package' is :straight-aware when possible.
(unless (and (require 'use-package nil 'noerror)
             (fboundp 'use-package-normalize/:straight))
  (when (fboundp 'straight-use-package)
    (condition-case err
        (progn
          (straight-use-package 'use-package)
          (require 'use-package nil 'noerror))
      (error
       (display-warning
        'core
        (format "Failed bootstrapping use-package via straight: %s" (error-message-string err))
        :warning)))))

;; Fallback to a harmless no-op macro when :straight-aware use-package is unavailable.
(unless (and (featurep 'use-package)
             (fboundp 'use-package-normalize/:straight))
  (display-warning
   'core
   "use-package with :straight support not available; package declarations are skipped"
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
