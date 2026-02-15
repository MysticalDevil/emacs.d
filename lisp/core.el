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

;; Ensure `use-package' is managed by straight, so `:straight' works as expected.
(when (fboundp 'straight-use-package)
  (condition-case err
      (straight-use-package 'use-package)
    (error
     (display-warning
      'core
      (format "Failed bootstrapping use-package via straight: %s" (error-message-string err))
      :warning))))

;; Fallback to a harmless no-op macro when :straight-aware use-package is unavailable.
(unless (and (require 'use-package nil 'noerror)
             (fboundp 'use-package-normalize/:straight))
  (display-warning
   'core
   "use-package with :straight support not available; package declarations are skipped"
   :warning)
  (defmacro use-package (&rest _args)
    "Fallback no-op when `use-package' is unavailable."
    nil))

;; Ensure user-local binaries are discoverable in all Emacs launch modes.
(defun my/prepend-local-bin-to-path ()
  "Prepend ~/.local/bin to both `exec-path' and PATH when available."
  (let ((local-bin (expand-file-name ".local/bin" (or (getenv "HOME") "~"))))
    (when (file-directory-p local-bin)
      (add-to-list 'exec-path local-bin)
      (unless (member local-bin (split-string (or (getenv "PATH") "") path-separator t))
        (setenv "PATH" (concat local-bin path-separator (or (getenv "PATH") "")))))))

(my/prepend-local-bin-to-path)

;; Sync shell PATH and environment for GUI/daemon Emacs sessions.
(use-package exec-path-from-shell
  :if (or (daemonp) (display-graphic-p))
  :config
  (dolist (var '("PATH" "GOPATH" "GOBIN"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize)
  ;; Keep ~/.local/bin available even if shell startup files omit it.
  (my/prepend-local-bin-to-path))

(provide 'core)
;;; core.el ends here
